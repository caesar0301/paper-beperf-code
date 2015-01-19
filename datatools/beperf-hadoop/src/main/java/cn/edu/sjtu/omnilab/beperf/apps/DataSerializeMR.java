package cn.edu.sjtu.omnilab.beperf.apps;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.apache.avro.mapred.AvroKey;
import org.apache.avro.mapreduce.AvroJob;
import org.apache.avro.mapreduce.AvroKeyOutputFormat;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import sjtu.omnilab.beperf.avro.HttpRecord;
import sjtu.omnilab.beperf.avro.NetflowRecord;
import cn.edu.sjtu.omnilab.beperf.utils.CSVParser;
import sjtu.omnilab.beperf.utils.MyUtilities;
import sjtu.omnilab.mr.input.MultilineInputFormat;
import sjtu.omnilab.mr.input.TextArrayWritable;

public class DataSerializeMR  extends Configured implements Tool  {
    
    private final static Logger logger = LoggerFactory.getLogger(DataSerializeMR.class);
    
    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new DataSerializeMR(), args);
        System.exit(res);
    }

    @Override
    public int run(String[] args) throws Exception {
        if (args.length < 2) {
            logger.error("Usage: DataSerialize <intputfile> <outputfolder>");
            System.exit(-1);
        }
        Configuration conf = new Configuration();
        Job job = new Job(getConf());
        job.setJobName("DataSerializeMapReduce");
        job.setJarByClass(DataSerializeMR.class);
        
        AvroJob.setOutputKeySchema(job, NetflowRecord.getClassSchema());
        
        FileInputFormat.setInputPaths(job, new Path(args[0]));
        Path outPath = new Path(args[1]);
        FileOutputFormat.setOutputPath(job, outPath);
        outPath.getFileSystem(conf).delete(outPath, true);

        job.setMapperClass(Map.class);
        job.setReducerClass(Reduce.class);
        
        job.setInputFormatClass(MultilineInputFormat.class);
        MultilineInputFormat.setMultilineStartString(job, "TCP:");
        job.setOutputFormatClass(AvroKeyOutputFormat.class);

        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(TextArrayWritable.class);
        
        job.setOutputKeyClass(AvroKey.class);
        job.setOutputValueClass(NullWritable.class);

        return job.waitForCompletion(true) ? 0 : 1;
    }
    
    public static class Map extends Mapper<LongWritable, TextArrayWritable, Text, TextArrayWritable> {
        
        @Override
        protected void map(LongWritable key, TextArrayWritable value, Context context) 
                                            throws IOException, InterruptedException {
            // Input: a network flow record with TCP and HTTP information
            // Output: User Mac and flow record pair
            Writable[] lines = value.get();
            if (lines.length > 0) {
                String firstLine = lines[0].toString();
                if (firstLine.startsWith("TCP:")) {
                    String[] parts = firstLine.split(" ");
                    if (parts.length > 2) {
                        String macStr = parts[1].trim();
                        if (!macStr.equals("N/A") && !macStr.equals("-")) {
                            macStr = macStr.replaceAll(":", "");
                            if ( macStr.length() > 0)
                                context.write(new Text(macStr), value);
                        }
                    }
                }
            }
        }
    }
    
    
    public static class Reduce extends Reducer< Text, TextArrayWritable, AvroKey<NetflowRecord>, NullWritable> {
        AvroKey<NetflowRecord> outputKey = new AvroKey<NetflowRecord>();

        @Override
        public void reduce(Text key, Iterable<TextArrayWritable> values, Context context) 
                                            throws IOException, InterruptedException {
            logger.info(String.format("user %s", key.toString()));
            for (TextArrayWritable record : values ){
                NetflowRecord netflowRecord = assembleNetflowRecord(record);
                if ( netflowRecord !=  null ) {
                    outputKey.datum(netflowRecord);
                    context.write(outputKey, null);
                }
                //context.progress();
            } 
        }
    }
    
    /**
     * Core method to create the NetflowRecord instances.
     * Assemble the TextArrayWritable object into a NetflowRecord instance.
     * @param textArray
     * @return
     */
    public static NetflowRecord assembleNetflowRecord(TextArrayWritable textArray) {
        // TODO(chen): Add test utility to this routine.
        // Some specific flags
        final String TSTAT_NONSTRING = "-";
        final String MY_NONSTRING = "N/A";
        final String TCP_RECORD_FLAG = "TCP:";
        final String HTTP_RECORD_FLAG = "HTTP:";

        // Create a new record
        NetflowRecord netflowRecord = new NetflowRecord();
        List<HttpRecord> httpRecordList = new LinkedList<HttpRecord>();
        CSVParser tcpCsvParser = new CSVParser(' ');
        CSVParser httpCsvParser = new CSVParser(' ', '\"');

        // First line represents TCP, and following lines record HTTP messages transfered.
        for( Writable text : textArray.get()){
            // Process each line
            String line = text.toString();  
            // Parse TCP logs.
            if ( line.startsWith(TCP_RECORD_FLAG)){
                // Ref project: https://github.com/caesar0301/PcapEx
                line = StringUtils.strip(line.split(":", 2)[1], " \r\n");
                // Parse MAC address, associated AP and layer-7 protocol;
                String devMacAddr = "";
                String assoApName = "";
                String layer7proto = "";
                String tstat_line = "";
                try{
                    String[] chops = line.split("\\s+", 4);
                    devMacAddr = StringUtils.strip(chops[0], " \r\n");
                    assoApName = StringUtils.strip(chops[1], " \r\n");
                    layer7proto = StringUtils.strip(chops[2], " \r\n");
                    tstat_line = StringUtils.strip(chops[3], " \r\n");
                } catch(IndexOutOfBoundsException e ){
                    logger.debug("TCP parser index out of bound: "+line);
                } catch ( Exception e ){
                    logger.error("Unknown error when parsing TCP logs: "+line+"\n"+e);
                    return null;
                }
                
                if ( devMacAddr.length() > 0 ){
                    try{ netflowRecord.setDevMacAddr(devMacAddr.equals(MY_NONSTRING) ? null : devMacAddr);}
                    catch(Exception e) {netflowRecord.setDevMacAddr(null);}
                }
                
                if ( assoApName.length() > 0 ){
                    try { netflowRecord.setAssocApName(assoApName.equals(MY_NONSTRING) ? null : assoApName);} 
                    catch(Exception e) {netflowRecord.setAssocApName(null);}
                }
                
                if ( layer7proto.length() > 0 ){
                    try { netflowRecord.setFlowL7proto(layer7proto.equals(MY_NONSTRING) ? null : layer7proto);} 
                    catch(Exception e) {netflowRecord.setFlowL7proto(null);}
                }                
                
                String[] parts = new String[0];
                if ( tstat_line.length() > 0 ){
                    try{ parts = tcpCsvParser.parseLine(tstat_line); }
                    catch ( IOException e) {
                        parts = tstat_line.split("\\s+");
                        logger.warn(e.toString());
                        logger.warn("Deperacated TCP parser manually ("+parts.length + "): "+line);
                    }
                }
                
                if ( parts.length > 0 ){    // exclude the absent TCP records
                    String field = "";
                    try{ // Src fields
                        field = parts[0];
                        try{ netflowRecord.setSrcIpAddr(field.equals(TSTAT_NONSTRING) ? null : MyUtilities.convertIpString2Long(field)); }
                        catch (NumberFormatException e) {
                            logger.error(e.toString()+", "+line);
                            // A known error in tagged logs where a field shifts left
                            // when the associated AP is absent.
                            // TODO(qsw): remove bugs in program to tag TCP with userMAC and AP;
                            return null;
                        }

                        field = parts[1];
                        try{ netflowRecord.setSrcTcpPort(field.equals(TSTAT_NONSTRING) ? null :Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[2];
                        try{ netflowRecord.setSrcTotPkts(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[3];
                        netflowRecord.setSrcHasRst(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[4];
                        try{ netflowRecord.setSrcAckNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[5];
                        try{ netflowRecord.setSrcPurAckNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[6];
                        try{ netflowRecord.setSrcPylBytUnq(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[7];
                        try{ netflowRecord.setSrcPylPkt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[8];
                        try{ netflowRecord.setSrcPylByt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[9];
                        try{ netflowRecord.setSrcPktRx(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[10];
                        try{ netflowRecord.setSrcBytRx(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[11];
                        try{ netflowRecord.setSrcPktOos(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[12];
                        try{ netflowRecord.setSrcTotSyn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[13];
                        try{ netflowRecord.setSrcTotFin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[14];
                        netflowRecord.setSrcHasWs(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[15];
                        netflowRecord.setSrcHasTs(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[16];
                        try{ netflowRecord.setSrcWsVal(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[17];
                        netflowRecord.setSrcHasSack(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[18];
                        try{ netflowRecord.setSrcSackNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[19];
                        try{ netflowRecord.setSrcMss(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[20];
                        try{ netflowRecord.setSrcSgmMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[21];
                        try{ netflowRecord.setSrcSgmMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[22];
                        try{ netflowRecord.setSrcRwinMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[23];
                        try{ netflowRecord.setSrcRwinMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[24];
                        try{ netflowRecord.setSrcRwinZero(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[25];
                        try{ netflowRecord.setSrcCwinMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[26];
                        try{ netflowRecord.setSrcCwinMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[27];
                        try{ netflowRecord.setSrcCwinInt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[28];
                        try{ netflowRecord.setSrcRttAvg(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[29];
                        try{ netflowRecord.setSrcRttMin(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); } 
                        catch (NumberFormatException e) {}

                        field = parts[30];
                        try{ netflowRecord.setSrcRttMax(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[31];
                        try{ netflowRecord.setSrcRttStd(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[32];
                        try{ netflowRecord.setSrcRttCnt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[33];
                        try{ netflowRecord.setSrcTtlMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[34];
                        try{ netflowRecord.setSrcTtlMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[35];
                        try{ netflowRecord.setSrcPktRxRTO(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[36];
                        try{ netflowRecord.setSrcPktRxFR(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[37];
                        try{ netflowRecord.setSrcPktReord(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[38];
                        try{ netflowRecord.setSrcPktNetDup(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[39];
                        try{ netflowRecord.setSrcPktUnk(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[40];
                        try{ netflowRecord.setSrcPktRxPrb(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[41];
                        try{ netflowRecord.setSrcPktRxRTOUnn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[42];
                        try{ netflowRecord.setSrcPktRxFRUnn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[43];
                        netflowRecord.setSrcHasRxSynDiffSn(field.equals("0") ? false : (field.equals("1") ? true : null));

                        // Dst
                        field = parts[44];
                        try{ netflowRecord.setDstIpAddr(field.equals(TSTAT_NONSTRING) ? null : MyUtilities.convertIpString2Long(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[45];
                        try{ netflowRecord.setDstTcpPort(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[46];
                        try{ netflowRecord.setDstTotPkts(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[47];
                        netflowRecord.setDstHasRst(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[48];
                        try{ netflowRecord.setDstAckNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[49];
                        try{ netflowRecord.setDstPurAckNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[50];
                        try{ netflowRecord.setDstPylBytUnq(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[51];
                        try{ netflowRecord.setDstPylPkt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[52];
                        try{ netflowRecord.setDstPylByt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[53];
                        try{ netflowRecord.setDstPktRx(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[54];
                        try{ netflowRecord.setDstBytRx(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[55];
                        try{ netflowRecord.setDstPktOos(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[56];
                        try{ netflowRecord.setDstTotSyn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[57];
                        try{ netflowRecord.setDstTotFin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[58];
                        netflowRecord.setDstHasWs(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[59];
                        netflowRecord.setDstHasTs(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[60];
                        try{ netflowRecord.setDstWsVal(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[61];
                        netflowRecord.setDstHasSack(field.equals("0") ? false : (field.equals("1") ? true : null));

                        field = parts[62];
                        try{ netflowRecord.setDstSackNum(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[63];
                        try{ netflowRecord.setDstMss(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[64];
                        try{ netflowRecord.setDstSgmMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[65];
                        try{ netflowRecord.setDstSgmMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[66];
                        try{ netflowRecord.setDstRwinMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[67];
                        try{ netflowRecord.setDstRwinMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[68];
                        try{ netflowRecord.setDstRwinZero(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[69];
                        try{ netflowRecord.setDstCwinMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[70];
                        try{ netflowRecord.setDstCwinMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[71];
                        try{ netflowRecord.setDstCwinInt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[72];
                        try{ netflowRecord.setDstRttAvg(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[73];
                        try{ netflowRecord.setDstRttMin(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[74];
                        try{ netflowRecord.setDstRttMax(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[75];
                        try{ netflowRecord.setDstRttStd(field.equals(TSTAT_NONSTRING) ? null : Double.parseDouble(field)/1000); }
                        catch (NumberFormatException e) {}

                        field = parts[76];
                        try{ netflowRecord.setDstRttCnt(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[77];
                        try{ netflowRecord.setDstTtlMin(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[78];
                        try{ netflowRecord.setDstTtlMax(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[79];
                        try{ netflowRecord.setDstPktRxRTO(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[80];
                        try{ netflowRecord.setDstPktRxFR(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[81];
                        try{ netflowRecord.setDstPktReord(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[82];
                        try{ netflowRecord.setDstPktNetDup(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[83];
                        try{ netflowRecord.setDstPktUnk(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[84];
                        try{ netflowRecord.setDstPktRxPrb(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[85];
                        try{ netflowRecord.setDstPktRxRTOUnn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[86];
                        try{ netflowRecord.setDstPktRxFRUnn(field.equals(TSTAT_NONSTRING) ? null : Long.parseLong(field)); }
                        catch (NumberFormatException e) {}

                        field = parts[87];
                        netflowRecord.setDstHasRxSynDiffSn(field.equals("0") ? false : (field.equals("1") ? true : null));

                        // Flow timestamps in seconds
                        double flowStartTime = -1;
                        field = parts[97];
                        try{ 
                        	flowStartTime = Double.parseDouble(field)/1000; 
                        }catch( NumberFormatException e) {
                            logger.error(String.format("%s, Parse flow start time error: %s",  e.toString(), parts[99]));
                        }
                        
                        if( flowStartTime != -1){
                            netflowRecord.setFlowStartTime(flowStartTime);

                            field = parts[88];
                            try{ netflowRecord.setFlowEndTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            // Skip 89 and 90 fields about relative times since first segment ever.
                            // Note: Flow connection duration refers to the 3HS interval and is fetched from HTTP records if available

                            field = parts[91];
                            try{ netflowRecord.setSrc1stPylTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            field = parts[92];
                            try{ netflowRecord.setDst1stPylTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            field = parts[93];
                            try{ netflowRecord.setSrcPylLstTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            field = parts[94];
                            try{ netflowRecord.setDstPylLstTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            field = parts[95];
                            try{ netflowRecord.setSrc1stAckNoSynTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}

                            field = parts[96];
                            try{ netflowRecord.setDst1stAckNoSynTime(field.equals(TSTAT_NONSTRING) ? null : flowStartTime+Double.parseDouble(field)/1000); }
                            catch(NumberFormatException e ) {}
                        }
                    } catch(IndexOutOfBoundsException e ){
                        logger.debug("TCP parser index out of bound: "+line);
                    } catch ( Exception e ){
                        logger.error("Unknown error when parsing TCP logs: "+line+"\n"+e);
                        return null;
                    }
                }
            }
            
            // Parse HTTP logs.
            if ( line.startsWith(HTTP_RECORD_FLAG)) {
                // Ref project: https://github.com/caesar0301/PcapEx
                line = StringUtils.strip(line.split(":", 2)[1], " \r\n");
                // Extract HTTP fields
                String[] parts = new String[0];
                try{
                    parts = httpCsvParser.parseLine(line);
                } catch (IOException e) {
                    List<String> tempParts = new LinkedList<String>();
                    String[] chops = line.split("\\s+", 18); // 17 unquoted fileds, and other (") quoted fields
                    // NOTE: manully splitting
                    if ( chops.length ==  18 ){
                        for ( int i = 0 ; i<17; i++){
                            tempParts.add(chops[i]);
                        }
                        String[] subchops = chops[17].split("\"\\s+\"");
                        for ( String field : subchops ){
                            tempParts.add(StringUtils.strip(field, " \t\r\n"));
                        }
                    }
                    parts = (String[]) tempParts.toArray(new String[tempParts.size()]);
                    logger.warn(e.toString());
                    logger.warn("Deperacated HTTP parser manually (" + parts.length + "):" + line);
                }

                if ( parts.length > 0 ){
                    try{
                        HttpRecord httpRecord = new HttpRecord();          
                        long srcIpAddr = -1;
                        try{ srcIpAddr = MyUtilities.convertIpString2Long(parts[0]); }
                        catch( NumberFormatException e) {}
                        
                        if ( netflowRecord.getSrcIpAddr() != null ) {
                            httpRecord.setConnDir( netflowRecord.getSrcIpAddr() == srcIpAddr ? true : false);
                        } else if ( srcIpAddr != -1 ){
                            netflowRecord.setSrcIpAddr(srcIpAddr);
                            httpRecord.setConnDir(true);
                        }
                        
                        if ( netflowRecord.getSrcTcpPort() == null ){
                            try { netflowRecord.setSrcTcpPort(Long.parseLong(parts[1])); }
                            catch ( NumberFormatException e ) {}
                        }
                        
                        if ( netflowRecord.getDstIpAddr() == null ){
                            try { netflowRecord.setDstIpAddr(MyUtilities.convertIpString2Long(parts[2])); }
                            catch ( NumberFormatException e ) {}
                        }
                        
                        if ( netflowRecord.getDstTcpPort() == null ){
                            try { netflowRecord.setDstTcpPort(Long.parseLong(parts[3])); }
                            catch ( NumberFormatException e ) {}
                        }
                        
                        // connection time ignored: parts[4]
                        if ( netflowRecord.getFlowStartTime() ==  null) {
                            try { netflowRecord.setFlowStartTime( parseHttpTimeFixed(parts[5]));}
                            catch (NumberFormatException e ) {}
                        }
                        
                        if ( netflowRecord.getFlowEndTime() == null ){
                            try { netflowRecord.setFlowEndTime( parseHttpTimeFixed(parts[6])); } 
                            catch ( NumberFormatException e ) {}
                        }
                        
                        if ( netflowRecord.getFlowConnDur() == null ){
                            try { netflowRecord.setFlowConnDur( parseHttpTimeFixed(parts[7])); }
                            catch ( NumberFormatException e) {}
                        }
                        
                        try { httpRecord.setIdleDur0( parseHttpTimeFixed(parts[8])); }
                        catch ( NumberFormatException e ) {}
                        
                        try { httpRecord.setReqTime( parseHttpTimeFixed(parts[9])); }
                        catch ( NumberFormatException e ) {
                            logger.error(e.toString()+", Parsing request time error: "+parts[9]);
                        }
                        
                        try { httpRecord.setReqDur( parseHttpTimeFixed(parts[10])); }
                        catch ( NumberFormatException e ) {}
                        
                        try { httpRecord.setRspTime( parseHttpTimeFixed(parts[11])); }
                        catch ( NumberFormatException e ) {}
                        
                        // response delay ignored (parts[12]) as it can be derived from 
                        // the combination of request duration and response timestamp.
                        try { httpRecord.setRspDur( parseHttpTimeFixed(parts[13])); }
                        catch ( NumberFormatException e ) {}
                        
                        try { httpRecord.setIdleDur1( parseHttpTimeFixed(parts[14])); }
                        catch ( NumberFormatException e ) {}
                        
                        try { httpRecord.setReqPylByt( Long.parseLong(parts[15])); }
                        catch ( NumberFormatException e ) {}
                        
                        try { httpRecord.setRspPylByt( Long.parseLong(parts[16])); }
                        catch ( NumberFormatException e ) {}
                        
                        // Request
                        httpRecord.setReqMethod( parts[17].equals(MY_NONSTRING) ? null : parts[17]);
                        httpRecord.setReqUrl( parts[18].equals(MY_NONSTRING) ? null : parts[18]);
                        httpRecord.setReqProto( parts[19].equals(MY_NONSTRING) ? null : parts[19]);
                        httpRecord.setReqHost( parts[20].equals(MY_NONSTRING) ? null : parts[20]);
                        httpRecord.setReqUa( parts[21].equals(MY_NONSTRING) ? null : parts[21]);
                        httpRecord.setReqReferer( parts[22].equals(MY_NONSTRING) ? null : parts[22]);
                        httpRecord.setReqConn( parts[23].equals(MY_NONSTRING) ? null : parts[23]);
                        httpRecord.setReqKeepAlive( parts[24].equals(MY_NONSTRING) ? null : parts[24]);
                        
                        // Response
                        httpRecord.setRspProto( parts[25].equals(MY_NONSTRING) ? null : parts[25]);
                        httpRecord.setRspCode( parts[26].equals(MY_NONSTRING) ? null : parts[26]);
                        httpRecord.setRspServer( parts[27].equals(MY_NONSTRING) ? null : parts[27]);
                        httpRecord.setRspContentLen( parts[28].equals(MY_NONSTRING) ? null : parts[28]);
                        httpRecord.setRspContentType( parts[29].equals(MY_NONSTRING) ? null : parts[29]);
                        httpRecord.setRspContentEncode( parts[30].equals(MY_NONSTRING) ? null : parts[30]);
                        httpRecord.setRspEtag( parts[31].equals(MY_NONSTRING) ? null : parts[31]);
                        httpRecord.setRspCacheCtrl( parts[32].equals(MY_NONSTRING) ? null : parts[32]);
                        httpRecord.setRspLastMod( parts[33].equals(MY_NONSTRING) ? null : parts[33]);
                        httpRecord.setRspAge( parts[34].equals(MY_NONSTRING) ? null : parts[34]);
                        httpRecord.setRspExpire( parts[35].equals(MY_NONSTRING) ? null : parts[35]);
                        httpRecord.setRspConn( parts[36].equals(MY_NONSTRING) ? null : parts[36]);
                        httpRecord.setRspKeepAlive( parts[37].equals(MY_NONSTRING) ? null : parts[37]);
                        
                        // Add to netflow record
                        httpRecordList.add(httpRecord);
                    } catch(IndexOutOfBoundsException e ){
                        logger.debug("HTTP parser index out of bound: "+line);
                    } catch ( Exception e ){
                        logger.error("Unknown error when parsing HTTP logs: "+line+"\n"+e.toString());
                        return null;
                    }
                }
            }
        }
        
        netflowRecord.setHttpPairs(httpRecordList);
        return netflowRecord;
    }
    
    /**
     * Routine to fix HTTP timestamp output by Justniffer.
     * I once found some request sequences in specific TCP flows whose HTTP request timestamps
     * lost one digit when dumping microseconds, e.g., using 0.12345 instead of 0.012345. 
     * @param timeStr
     * @return
     */
    private static double parseHttpTimeFixed(String timeStr){
        double timestamp = -1;
        if ( timeStr.contains(".")) {
            String[] parts = timeStr.split("\\.");
            timestamp = Integer.parseInt(parts[0]);
            if ( parts[1].length() <= 3 )
                timestamp += Integer.parseInt(parts[1]) / 1000.0;
            else if (parts[1].length() <= 6)
                timestamp += Integer.parseInt(parts[1]) / 1000000.0;
        } else {
            timestamp = Integer.parseInt(timeStr);
        }
        return timestamp;
    }
}
