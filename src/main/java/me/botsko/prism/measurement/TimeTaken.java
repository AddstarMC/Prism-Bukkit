package me.botsko.prism.measurement;

import java.util.Calendar;
import java.util.TreeMap;

public class TimeTaken {

	/**
	 * 
	 */
	protected TreeMap<Long,String> eventsTimed = new TreeMap<Long,String>();
	
	
	/**
	 * 
	 * @return
	 */
	protected long getTimestamp(){
		Calendar lCDateTime = Calendar.getInstance();
		return lCDateTime.getTimeInMillis();
	}
	
	
	/**
	 * 
	 * @param eventname
	 */
	public void recordTimedEvent( String eventname ){
		eventsTimed.put(getTimestamp(), eventname);
		System.out.println(eventname);
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
	public TreeMap<Long,String> getEventsTimedList(){
		return eventsTimed;
	}
}
