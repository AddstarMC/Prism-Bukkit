package me.botsko.prism.measurement;

import java.util.Calendar;
import java.util.HashMap;

public class TimeTaken {

	/**
	 * 
	 */
	protected HashMap<Long,String> eventsTimed = new HashMap<Long,String>();
	
	
	/**
	 * 
	 * @return
	 */
	public long getTimestamp(){
		Calendar lCDateTime = Calendar.getInstance();
		return lCDateTime.getTimeInMillis();
	}
	
	
	/**
	 * 
	 * @param eventname
	 */
	public void recordTimedEvent( String eventname ){
		eventsTimed.put(getTimestamp(), eventname);
	}
	
	
	/**
	 * 
	 */
	public void resetEventList(){
		eventsTimed.clear();
	}
	
	
	/**
	 * 
	 * @return
	 */
	public HashMap<Long,String> getEventsTimedList(){
		return eventsTimed;
	}
}
