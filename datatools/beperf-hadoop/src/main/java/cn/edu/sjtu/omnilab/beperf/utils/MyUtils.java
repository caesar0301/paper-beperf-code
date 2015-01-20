package cn.edu.sjtu.omnilab.beperf.utils;

/**
 * Created by chenxm on 1/20/15.
 */
public class MyUtils {

    /**
     * Parse the long number of IPv4 address from dot-separated string
     *
     * @param IP
     * @return
     */
    public static long convertIpString2Long(String IP) {
        String[] addrArray = IP.split("\\.");
        if (addrArray.length != 4) {
            throw new NumberFormatException("Invalid IP string format: " + IP);
        }
        long num = 0;
        for (int i = 0; i < addrArray.length; i++) {
            int power = 3 - i;
            num += ((Integer.parseInt(addrArray[i]) % 256 * Math.pow(256, power)));
        }
        return num;
    }
}
