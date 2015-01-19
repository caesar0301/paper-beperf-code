package cn.edu.sjtu.omnilab.beperf.apps;

import java.io.IOException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import cn.edu.sjtu.omnilab.beperf.measures.RequiredFieldAbsentException;
import org.apache.avro.mapred.AvroKey;
import org.apache.avro.mapred.AvroValue;
import org.apache.avro.mapreduce.AvroJob;
import org.apache.avro.mapreduce.AvroKeyInputFormat;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import sjtu.omnilab.beperf.avro.NetflowRecord;
import cn.edu.sjtu.omnilab.beperf.measures.NetflowAnomaly;

public class AnomalyDetectMR extends Configured implements Tool {
	private final static Logger logger = LoggerFactory.getLogger(AnomalyDetectMR.class);
    
    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new AnomalyDetectMR(), args);
        System.exit(res);
    }

    @Override
    public int run(String[] args) throws Exception {
        if (args.length < 2) {
            logger.error("Usage: AnomalyDetect <intputfile> <outputfolder>");
            System.exit(-1);
        }
        Configuration conf = new Configuration();
        Job job = new Job(getConf());
        job.setJobName("AnomalyDetect");
        job.setJarByClass(AnomalyDetectMR.class);
        
        job.setInputFormatClass(AvroKeyInputFormat.class);
        AvroJob.setInputKeySchema(job, NetflowRecord.getClassSchema());
        
        job.setMapperClass(Map.class);
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(AvroValue.class);
        AvroJob.setMapOutputValueSchema(job, NetflowRecord.getClassSchema());
        
        job.setReducerClass(Reduce.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(NullWritable.class);
        
        job.setOutputFormatClass(TextOutputFormat.class);
        
        FileInputFormat.setInputPaths(job, new Path(args[0]));
        Path outPath = new Path(args[1]);
        FileOutputFormat.setOutputPath(job, outPath);
        outPath.getFileSystem(conf).delete(outPath, true);
        //LazyOutputFormat.setOutputFormatClass(job, AvroKeyOutputFormat.class);

        return job.waitForCompletion(true) ? 0 : 1;
    }
    
    public static class Map extends Mapper<AvroKey<NetflowRecord>, NullWritable,
										Text, AvroValue<NetflowRecord>>{
    	@Override
    	protected void map(AvroKey<NetflowRecord> key, NullWritable value, Context context) 
    									throws IOException, InterruptedException{
    		// Classify records by user's device MAC
    		CharSequence devMac = key.datum().getDevMacAddr();
    		if ( devMac != null ){
    			context.write(new Text(devMac.toString()), new AvroValue<NetflowRecord>(key.datum()));
    		}
    	}
    }
    
    public static class Reduce extends Reducer<Text, AvroValue<NetflowRecord>, Text, NullWritable>{
    	/**
    	 * Inner helper class
    	 * @author chenxm
    	 *
    	 */
    	private class SimpleFlowRecord implements Comparable<SimpleFlowRecord>{
    		double flowStartTime;
    		double flowEndTime;
    		boolean hasReset = false;
    		boolean isEligible = false;
    		boolean isInterrupted = false;
    		
			@Override
			public int compareTo(SimpleFlowRecord o) {
				if ( this.flowStartTime == o.flowStartTime)
					return 0;
				else
					return (this.flowStartTime < o.flowStartTime ? -1 : 1);
			}
    	}
    	
		@Override
    	protected void reduce(Text key, Iterable<AvroValue<NetflowRecord>> values, Context context) 
				throws IOException, InterruptedException{
    		List<SimpleFlowRecord> recordList = new LinkedList<SimpleFlowRecord>();
    		for ( AvroValue<NetflowRecord> record : values){
    			NetflowRecord originalRecord = record.datum();
    			if ( originalRecord.getFlowStartTime() == null ||
    				originalRecord.getFlowEndTime() == null )
    				continue;
    			SimpleFlowRecord simpleRecord = new SimpleFlowRecord();
    			simpleRecord.flowStartTime = originalRecord.getFlowStartTime();
    			simpleRecord.flowEndTime = originalRecord.getFlowEndTime();
    			//simpleRecord.hasReset = (originalRecord.getSrcHasRst() == null ? false : originalRecord.getSrcHasRst());
    			try {
    				simpleRecord.hasReset = NetflowAnomaly.clientReset(record.datum());
    				simpleRecord.isEligible = NetflowAnomaly.isEligible(record.datum());
					simpleRecord.isInterrupted = NetflowAnomaly.isInterrupted(record.datum());
				} catch (RequiredFieldAbsentException e) {
					//logger.info(String.format("Omit logs without required fields: %s", record.datum().toString()));
				}
    			recordList.add(simpleRecord);
    		}
    		
    		if ( recordList.size() > 0 ){
	    		Collections.sort(recordList);
	    		double startTime = recordList.get(0).flowStartTime;
	    		double endTime = recordList.get(recordList.size()-1).flowEndTime;
	    		
	    		// Output
	    		StringBuffer sb = new StringBuffer();
	    		sb.append(key.toString() + " ");
	    		sb.append(String.format("%.0f", startTime) + " ");
	    		sb.append(String.format("%.0f", endTime) + " ");
	    		for ( SimpleFlowRecord sfr : recordList ){
	    			if ( sfr.hasReset){
	    				sb.append(String.format("%.0f,%s,%s,%s",sfr.flowEndTime-startTime, 
	    						sfr.hasReset, sfr.isEligible, sfr.isInterrupted));
	    				sb.append(";");
	    			}
	    		}
	    		context.write(new Text(sb.toString()), NullWritable.get());
    		}
    	}
    }
}
