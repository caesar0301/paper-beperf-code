package cn.edu.sjtu.omnilab.beperf.measures;

/**
 * Exception about the absent fields in logs when perform some
 * calculations.
 * @author chenxm
 *
 */
public class RequiredFieldAbsentException extends Exception {
	public RequiredFieldAbsentException(String msg){
		super(msg);
	}
}
