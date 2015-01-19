/*
Obtain protocol statistics in a day granularity and protocol granularity.

Usage:
cd tools/
pig -param input=path/to/input/file -param output=path/to/input/file thisscriptname.pig

Author Xiaming Chen
chenxm35@gmail.com
*/

DEFINE AvroStorage org.apache.pig.piggybank.storage.avro.AvroStorage;
DEFINE UnixToISO org.apache.pig.piggybank.evaluation.datetime.convert.UnixToISO();
DEFINE ISOToDay org.apache.pig.piggybank.evaluation.datetime.truncate.ISOToDay();

-- Load all flows
flows = load '$input' using  AvroStorage();

filtered = FILTER flows BY flowL7proto is not null and flowStartTime is not null;

-- Extract required fields
interested = FOREACH filtered {
    parts = STRSPLIT(flowL7proto, '/');
    GENERATE
    UnixToISO((long)(flowStartTime+8*3600)*1000) as time, 
    parts.$0 AS proto,
    (srcTotPkts+dstTotPkts) AS pkts,
    (srcPylByt+dstPylByt) AS byts;
};

gday = GROUP interested BY (ISOToDay(time), proto);
gday_clean = FILTER gday BY group.$1 is not null and group.$1 != '-';

--Group by protocol and day, and sum flow statistics
appDaySum = FOREACH gday_clean GENERATE FLATTEN(group),
        -- flows
        COUNT(interested) AS flw,
        -- KP
        (float)SUM(interested.pkts)/1024 AS pkt,
        -- MB
        (float)SUM(interested.byts)/1024/1024 AS byt;

appDayGrp = GROUP appDaySum BY $0;
appDayGrpSum = FOREACH appDayGrp GENERATE group,
        SUM(appDaySum.flw) as flw, 
        SUM(appDaySum.pkt) as pkt,
        SUM(appDaySum.byt) as byt;

appDaySumMgd = JOIN appDaySum BY $0, appDayGrpSum BY $0;  

appDayFinal = FOREACH appDaySumMgd GENERATE $0 .. $4,
        (float)appDaySum::flw/appDayGrpSum::flw, 
        (float)appDaySum::pkt/appDayGrpSum::pkt,
        (float)appDaySum::byt/appDayGrpSum::byt;

-- Format: time(day), protocol, flows, packets(K), bytes(M), flows(% in the day),packets(%),bytes(%)
STORE appDayFinal INTO '$output/appProtoStat.day.gz';


gproto = GROUP appDaySum BY $1;
appProtoSum = FOREACH gproto GENERATE $0 as proto, SUM(appDaySum.flw) as flw, SUM(appDaySum.pkt) as pkt, SUM(appDaySum.byt) as byt;

-- Calculate total traffic volumn
appGrpAll = GROUP appProtoSum ALL;
appSumAll = FOREACH appGrpAll GENERATE SUM(appProtoSum.flw), SUM(appProtoSum.pkt), SUM(appProtoSum.byt);

-- Calculate proportional values
prop = FOREACH appProtoSum GENERATE *, (float)flw/appSumAll.$0, (float)pkt/appSumAll.$1, (float)byt/appSumAll.$2;

-- Format: protocol, flows, packets(K), bytes(M), flow (%), packets (%), bytes (%)
STORE prop INTO '$output/appProtoStat.all.gz';