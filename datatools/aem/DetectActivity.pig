/*
The implementation of Activity-Entity Model (AEM) in pig.

Usage:
	pig -param input=input/path -param output=output/path thisscriptname.pig

Input: traffic logs serialized (by `beperf-hadoop`) in avro format.
Output: traffic logs marked with activity and session IDs.

@Author: Xiaming Chen - chenxm35@gmail.com
    2013-12-27
*/

-- for debugging
-- %DECLARE input '../../data/sample-large.avro'
-- %DECLARE output '../../output/http.out'

DEFINE UnixToISO org.apache.pig.piggybank.evaluation.datetime.convert.UnixToISO();
DEFINE AvroStorage org.apache.pig.piggybank.storage.avro.AvroStorage();
DEFINE GenUUIDRand com.piggybox.uuid.GenUUIDRand();
DEFINE StripUrlLeft com.piggybox.http.StripUrlLeft;
DEFINE UrlFromHostUri com.piggybox.http.UrlFromHostUri;
DEFINE DetectActivity com.piggybox.omnilab.aem.DetectActivity('2s');
DEFINE Sessionize datafu.pig.sessions.Sessionize('15m');
DEFINE Quantile datafu.pig.stats.Quantile('5');

SET debug off
SET job.name 'Running AEM alg.'

-- Load all flows
flows = LOAD '$input' USING AvroStorage();

-- Append network behavior measures
flowqos = FOREACH flows GENERATE
	-- all flow fields
	$0 ..,
    -- general bandwidth
	(srcPylByt+dstPylByt)/(flowEndTime-flowStartTime) as gnlBw,
	-- efficient bandwidth
	(srcPylByt+dstPylByt)/(MAX(TOBAG(srcPylLstTime, dstPylLstTime))-
    MIN(TOBAG(src1stPylTime, dst1stPylTime))) as effBw,
	(srcBytRx+dstBytRx) AS rxByt, -- RX bytes
	(srcPktRx+dstPktRx) AS rxPkt, -- RX packets
	(srcPktOos+dstPktOos) AS oosPkt, -- OOS packets
	GenUUIDRand() AS flowID;

-- Filter out invalid results
flowhttp = FILTER flowqos BY SIZE(httpPairs) > 0 and
    gnlBw is not null and
    effBw is not null;

-- Flatten HTTP messages
rawhttp = FOREACH flowhttp GENERATE
    devMacAddr as userID,
    assocApName,srcIpAddr,dstIpAddr,srcTcpPort,
    dstTcpPort,flowStartTime,flowEndTime,
    flowConnDur,srcHasRst,dstHasRst,srcTotFin,
    dstTotFin,srcRttAvg,dstRttAvg,srcRttStd,
    dstRttStd,srcPylByt,dstPylByt,srcPylLstTime,
    dstPylLstTime,srcTtlMax,dstTtlMax,gnlBw,
    effBw,rxByt,rxPkt,oosPkt,flowID,
    FLATTEN(httpPairs);

-- Filter out HTTP msgs without request time
httpvld = FILTER rawhttp BY
    NOT(reqTime is null OR reqUrl is null OR reqUa is null);

-- Filter out unnecce fields
http = FOREACH httpvld GENERATE
    userID .. flowID, -- Net QoS
	connDir,reqTime,reqDur,idleDur0,
    -- Note: idelDur0 records the duration from connection
    -- establishment to first request byte.
    rspTime,rspDur,idleDur1,reqPylByt,
    rspPylByt,reqMethod,reqUrl,reqProto,
    reqHost,reqUa,reqReferer,rspCode,
    rspServer,rspContentType,
    GenUUIDRand() AS httpID;

/*
Detect sessions and activities
DESCRIBE http;

0-5, userID:chararray, assocApName:chararray, srcIpAddr:long,
dstIpAddr:long, srcTcpPort:long, dstTcpPort:long,

6-11, flowStartTime:double, flowEndTime:double, flowConnDur:double,
srcHasRst:boolean, dstHasRst:boolean, srcTotFin:long,

12-17, dstTotFin:long, srcRttAvg:double, dstRttAvg:double,
srcRttStd:double, dstRttStd:double, srcPylByt:long,

18-22, dstPylByt:long, srcPylLstTime:double, dstPylLstTime:double,
srcTtlMax:long, dstTtlMax:long,

23-29, gnlBw:double, effBw:double, rxByt:long, rxPkt:long,
oosPkt:long, flowID:chararray, connDir:boolean,

30-35, reqTime:double, reqDur:double, idleDur0:double,
rspTime:double, rspDur:double, idleDur1:double,

36-41, reqPylByt:long, rspPylByt:long, reqMethod:chararray,
reqUrl:chararray, reqProto:chararray, reqHost:chararray,

42-47, reqUa:chararray, reqReferer:chararray, rspCode:chararray,
rspServer:chararray, rspContentType:chararray, httpID:chararray
*/

-- Group HTTP msgs by (User) or (devMacAddr)
userlogs = GROUP http BY userID;

-- Sessionize logs
requests = FOREACH userlogs {
	brief = FOREACH http GENERATE
		UnixToISO((long)(reqTime+8*3600)*1000) as isotime,
		httpID as hid,
        reqTime as ST, -- start time
		(rspTime+rspDur) as ET, -- end time
		StripUrlLeft(UrlFromHostUri(reqHost, reqUrl)) as url,
		StripUrlLeft(reqReferer) as referrer,
		rspContentType as type,
        reqUa as ua;
	ordered = ORDER brief BY ST;
	sessions = Sessionize(ordered);
	GENERATE FLATTEN(sessions) AS
        (isotime, hid, ST, ET, url,
        referrer, type, ua, sessionID);
}
requests = FOREACH requests GENERATE
    ST, ET, url ,referrer, type,
    hid, ua, sessionID;

-- Group http msgs by (SessionID, UserAgent) into individual sessions.
uasessions = GROUP requests BY (sessionID, ua);

-- Detect activities via AEM model.
activities = FOREACH uasessions {
	ordered = ORDER requests BY ST;
	acts = DetectActivity(ordered);
	GENERATE FLATTEN(acts) AS
        (ST, ET, url ,referrer, type,
        hid, ua, sessionID, activityID);
}
activities = FOREACH activities GENERATE hid, sessionID, activityID;

-- merge http with activities by activityID (RIGHT OUTER join is
-- involved because we may make filtering to sessions/activities.)
http = JOIN http BY httpID, activities BY hid;

-- remove unuseful fields
-- new fields added: 48-49,  activityID:chararray, sessionID:chararray
http = FOREACH http GENERATE
    userID .. httpID,
    activityID, sessionID;
http = order http by reqTime;

STORE http INTO '$output';