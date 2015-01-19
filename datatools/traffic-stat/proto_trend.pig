/* Get the layer-7 protocol traffic volumn at both day and hour granularity.
Usage:
cd tools/
pig -param input=path/to/input/file -param output=path/to/input/file thisscriptname.pig
Author Xiaming Chen
chenxm35@gmail.com
*/
DEFINE AvroStorage org.apache.pig.piggybank.storage.avro.AvroStorage;
DEFINE UnixToISO org.apache.pig.piggybank.evaluation.datetime.convert.UnixToISO();
DEFINE ISOToDay org.apache.pig.piggybank.evaluation.datetime.truncate.ISOToDay();
DEFINE ISOToHour org.apache.pig.piggybank.evaluation.datetime.truncate.ISOToHour();


-- Load all flows
flows = load '$input' using  AvroStorage();

-- Filter out flows without valid start time
flows_with_time = filter flows by flowStartTime is not null;

-- Extract interested fields
interested = foreach flows_with_time generate 
    UnixToISO((long)(flowStartTime+8*3600)*1000) as time, 
    devMacAddr as umac,
    assocApName as apname, flowL7proto as proto, 
    srcTotPkts as srcpkts, dstTotPkts as dstpkts, srcTotPkts+dstTotPkts as packets,
    srcPylByt as srcbyts, dstPylByt as dstbyts, srcPylByt+dstPylByt as bytes;

-- Format timestamp by adding two more fields about hour and day.
unified = foreach interested generate ISOToDay(time) as day, 
    ISOToHour(time) as hour, time ..;

-- Group by hour and protocol
ghour = group unified by (hour, proto);
ghour_clean = filter ghour by group.proto is not null and group.proto != '-';

-- Group by day and protocol
gday = group unified by (day, proto);
gday_clean = filter gday by group.proto is not null and group.proto != '-';

-- Calculate hourly traffic statistics and order by time
hour_trend = foreach ghour_clean generate flatten(group), 
    SUM(unified.srcpkts), SUM(unified.dstpkts), SUM(unified.packets), 
    SUM(unified.srcbyts), SUM(unified.dstbyts), SUM(unified.bytes);
hour_trend_ord = order hour_trend by hour ASC, proto ASC;

-- Calculate daily traffic statistics and order by time
day_trend = foreach gday_clean generate flatten(group), 
    SUM(unified.srcpkts), SUM(unified.dstpkts), SUM(unified.packets), 
    SUM(unified.srcbyts), SUM(unified.dstbyts), SUM(unified.bytes);
day_trend_ord = order day_trend by day ASC, proto ASC;

-- Dump to disk
store hour_trend_ord into '$output/protoTrendHourly.out' using PigStorage; 
store day_trend_ord into '$output/protoTrendDaily.out' using PigStorage;