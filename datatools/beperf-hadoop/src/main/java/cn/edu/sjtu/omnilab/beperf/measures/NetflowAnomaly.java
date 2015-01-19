package cn.edu.sjtu.omnilab.beperf.measures;

import sjtu.omnilab.beperf.avro.NetflowRecord;

public class NetflowAnomaly {
	private static final int alpha = 1;
	private static final int beta = 0;
	
	/**
	 * Flow level anomaly about reset: client perspective
	 * @param r
	 * @return
	 * @throws RequiredFieldAbsentException
	 */
	public static boolean clientReset(NetflowRecord r) throws RequiredFieldAbsentException{
		if ( r.getSrcHasRst() == null ){
			throw new RequiredFieldAbsentException("Required fields are absent " +
					"in logs when detecting the clinet reset action.");
		}
		return r.getSrcHasRst();
	}
	
	/**
	 * Flow level anomaly: client-only perspective
	 * @param r
	 * @return
	 * @throws RequiredFieldAbsentException
	 */
	public static boolean isEligible(NetflowRecord r) throws RequiredFieldAbsentException{
		// Implementing the method proposed by Rossi et al. (2003)
		// Paper: User patience and the web: a hands-on investigation.
		try {
			return !(r.getDstTotFin()>0 || r.getDstHasRst()) &&
					r.getDstPylByt()>0 && r.getSrcHasRst();
		} catch (NullPointerException e) {
			throw new RequiredFieldAbsentException("Required fields are absent " +
					"in logs when calculating the eligibility.");
		}
	}
	
	/**
	 * Flow level anomaly: client-only perspective && transfer is interrupted.
	 * @param r
	 * @return
	 * @throws RequiredFieldAbsentException
	 */
	public static boolean isInterrupted(NetflowRecord r) throws RequiredFieldAbsentException {
		try {
			boolean flag = false;
			double rtt_avg = r.getSrcRttAvg() + r.getDstRttAvg();
			// Assuming the covariance between SRC_RTT and DST_RTT is zero,
			double rtt_std = Math.sqrt(Math.pow(r.getSrcRttStd(),2) + Math.pow(r.getDstRttStd(), 2));
			double t_gap = r.getFlowEndTime() - r.getDstPylLstTime();
			double t_gap_hat = (double) t_gap / (alpha * rtt_avg + beta * rtt_std);
			if ( isEligible(r) && t_gap_hat < 1)
				flag = true;
			return flag;
		} catch (NullPointerException e) {
			throw new RequiredFieldAbsentException("Required fields are absent " +
					"in logs when calculating the if-interrupted.");
		}
	}
}
