package cn.edu.sjtu.omnilab.beperf.apps;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.avro.mapred.AvroKey;
import org.apache.avro.mapred.AvroValue;
import org.apache.avro.mapreduce.AvroJob;
import org.apache.avro.mapreduce.AvroKeyInputFormat;
import org.apache.avro.mapreduce.AvroKeyOutputFormat;
import org.apache.avro.mapreduce.AvroMultipleOutputs;
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
import org.apache.hadoop.mapreduce.lib.output.LazyOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import sjtu.omnilab.beperf.avro.NetflowRecord;

public class UserClassifyMR extends Configured implements Tool {
	
	private final static Logger logger = LoggerFactory.getLogger(UserClassifyMR.class);
    
    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new UserClassifyMR(), args);
        System.exit(res);
    }

    @Override
    public int run(String[] args) throws Exception {
        if (args.length < 2) {
            logger.error("Usage: UserClassify <intputfile> <outputfolder>");
            System.exit(-1);
        }
        Configuration conf = new Configuration();
        Job job = new Job(getConf());
        job.setJobName("UserClassify");
        job.setJarByClass(UserClassifyMR.class);
        
        job.setInputFormatClass(AvroKeyInputFormat.class);
        AvroJob.setInputKeySchema(job, NetflowRecord.getClassSchema());
        
        job.setMapperClass(MyAvroMap.class);
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(AvroValue.class);
        AvroJob.setMapOutputValueSchema(job, NetflowRecord.getClassSchema());
        
        job.setReducerClass(MyAvroReduce.class);
        job.setOutputKeyClass(AvroKey.class);
        AvroJob.setOutputKeySchema(job, NetflowRecord.getClassSchema());
        job.setOutputValueClass(NullWritable.class);
        
        job.setOutputFormatClass(AvroKeyOutputFormat.class);
        
        FileInputFormat.setInputPaths(job, new Path(args[0]));
        Path outPath = new Path(args[1]);
        FileOutputFormat.setOutputPath(job, outPath);
        outPath.getFileSystem(conf).delete(outPath, true);
        LazyOutputFormat.setOutputFormatClass(job, AvroKeyOutputFormat.class);
        
        return job.waitForCompletion(true) ? 0 : 1;
    }
    
    public static class MyAvroMap extends Mapper<AvroKey<NetflowRecord>, NullWritable,
    									Text, AvroValue<NetflowRecord>>{
    	@Override
    	protected void map(AvroKey<NetflowRecord> key, NullWritable value, Context context) 
    									throws IOException, InterruptedException{
    		NetflowRecord flw = (NetflowRecord)key.datum();
    		CharSequence devMac = flw.getDevMacAddr();
    		if ( devMac != null ){
    			context.write(new Text(devMac.toString()),
    					  	  new AvroValue<NetflowRecord>(key.datum()));
    		}
    	}
    }
    
    
    public static class MyAvroReduce extends Reducer<Text, AvroValue<NetflowRecord>, 
    									AvroKey<NetflowRecord>, NullWritable>{
    	private AvroMultipleOutputs amos = null;
    	private Pattern mobilePattern, mobilePatternPlus;
    	private int FLOW_QUEUE_LENGTH = 1000;
    	
    	@Override
    	public void setup(Context context) {
    		amos = new AvroMultipleOutputs(context);
    		mobilePattern = Pattern.compile("android|(bb\\d+|meego).+mobile|avantgo|bada\\/|blackberry|blazer|compal|docomo|dolfin|dolphin|elaine|fennec|hiptop|iemobile|(hpw|web)os|htc( touch)?|ip(hone|od|ad)|iris|j2me|kindle( fire)?|lge |maemo|midp|minimo|mmp|netfront|nokia|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\\/|plucker|playstation|pocket|portalmmm|psp|series(4|6)0|symbian|silk-accelerated|skyfire|sonyericsson|treo|tablet|touch(pad)?|up\\.(browser|link)|vodafone|wap|webos|windows (ce|phone)|wireless|xda|xiino|zune", Pattern.CASE_INSENSITIVE);
    		//mobilePatternPlus = Pattern.compile("1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\\-(n|u)|c55\\/|capi|ccwa|cdm\\-|cell|chtm|cldc|cmd\\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\\-s|devi|dica|dmob|do(c|p)o|ds(12|\\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\\-|_)|g1 u|g560|gene|gf\\-5|g\\-mo|go(\\.w|od)|gr(ad|un)|haie|hcit|hd\\-(m|p|t)|hei\\-|hi(pt|ta)|hp( i|ip)|hs\\-c|ht(c(\\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\\-(20|go|ma)|i230|iac( |\\-|\\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\\/)|klon|kpt |kwc\\-|kyo(c|k)|le(no|xi)|lg( g|\\/(k|l|u)|50|54|\\-[a-w])|libw|lynx|m1\\-w|m3ga|m50\\/|ma(te|ui|xo)|mc(01|21|ca)|m\\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\\-2|po(ck|rt|se)|prox|psio|pt\\-g|qa\\-a|qc(07|12|21|32|60|\\-[2-7]|i\\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\\-|oo|p\\-)|sdk\\/|se(c(\\-|0|1)|47|mc|nd|ri)|sgh\\-|shar|sie(\\-|m)|sk\\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\\-|v\\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\\-|tdg\\-|tel(i|m)|tim\\-|t\\-mo|to(pl|sh)|ts(70|m\\-|m3|m5)|tx\\-9|up(\\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\\-|your|zeto|zte\\-", Pattern.CASE_INSENSITIVE);
    	}
    	
    	@Override
    	protected void reduce(Text key, Iterable<AvroValue<NetflowRecord>> values, Context context) 
    									throws IOException, InterruptedException{
    		// Store values for multiple traverses
    		List<NetflowRecord> flowRecordList = new LinkedList<NetflowRecord>();
    		Set<CharSequence> userAgentSet = new HashSet<CharSequence>();
    		// Check if user is utilizing mobile devices
			int mobAgentCnt = 0;
			boolean isMobile = false;
    		for( AvroValue<NetflowRecord> record : values) {
    			NetflowRecord thisRecord = record.datum();
    			flowRecordList.add(NetflowRecord.newBuilder(thisRecord).build());
    			if ( thisRecord.getHttpPairs().size() > 0 ){
    				CharSequence userAgent = thisRecord.getHttpPairs().get(0).getReqUa();
    				if ( userAgent != null && !userAgentSet.contains(userAgent)){
    					userAgentSet.add(userAgent);
    					Matcher matcher = mobilePattern.matcher(userAgent);
    					if ( matcher.find() ) { mobAgentCnt++; }
    				}
    			}
    			// TODO: Add OUI (Organizationally Unique Identifiers) detection to device types
    			if( mobAgentCnt >=2 ){ isMobile = true; break; }
    			if ( flowRecordList.size() >= FLOW_QUEUE_LENGTH) {break;}
    		}
    		if ( userAgentSet.size() == 1 && mobAgentCnt == 1) {isMobile = true;}
    		userAgentSet.clear();
			
			logger.info(String.format("User %s: mobile %b, est flows %d, mem %d/%d", key.toString(), isMobile,
			      flowRecordList.size(),Runtime.getRuntime().totalMemory(),Runtime.getRuntime().maxMemory()));
			
			long totalFlows = 0;
			String outputPath = isMobile ? "mobile/mobile" : "nonmobile/nonmobile";
			// Write stored in queue
			for ( NetflowRecord flowRecord : flowRecordList ) {
				amos.write(new AvroKey<NetflowRecord>(flowRecord), NullWritable.get(), outputPath);
				totalFlows++;
			}
			flowRecordList.clear();
			// Write left records
			for ( AvroValue<NetflowRecord> record : values ) {
				amos.write(new AvroKey<NetflowRecord>(record.datum()), NullWritable.get(), outputPath);
				totalFlows++;
			}
			
			logger.info(String.format("Total flows: %d", totalFlows));
    	}
    	
    	@Override
    	public void cleanup(Context context) throws IOException, InterruptedException {
    		if ( amos != null )
    			amos.close();
    		logger.info("reduce cleanup");
    	}
    }

}
