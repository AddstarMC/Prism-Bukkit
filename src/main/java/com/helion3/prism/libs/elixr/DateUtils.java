package com.helion3.prism.libs.elixr;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DateUtils {
	
	
	/**
	 * Returns a future timestamp based on the time format provided
	 * @param arg_value
	 * @return
	 */
	public static String translateTimeStringToFutureDate( String arg_value ){
		
		String dateFrom = null;

		Pattern p = Pattern.compile("([0-9]+)(s|h|m|d|w)");
		Calendar cal = Calendar.getInstance();

		String[] matches = TypeUtils.preg_match_all( p, arg_value );
		if(matches.length > 0){
			for(String match : matches){
	
				Matcher m = p.matcher( match );
				if(m.matches()){
					
					if( m.groupCount() == 2 ){
						
						int tfValue = Integer.parseInt( m.group(1) );
						String tfFormat = m.group(2);

						if(tfFormat.equals("w")){
							cal.add(Calendar.WEEK_OF_YEAR, tfValue);
						}
						else if(tfFormat.equals("d")){
							cal.add(Calendar.DAY_OF_MONTH, tfValue);
						}
						else if(tfFormat.equals("h")){
							cal.add(Calendar.HOUR, tfValue);
						}
						else if(tfFormat.equals("m")){
							cal.add(Calendar.MINUTE, tfValue);
						}
						else if(tfFormat.equals("s")){
							cal.add(Calendar.SECOND, tfValue);
						}
					}
				}
			}
			SimpleDateFormat form = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			dateFrom = form.format(cal.getTime());
		}
		return dateFrom;
	}
	
	
	/**
	 * Returns a past timestamp based on the time format provided
	 * @return
	 */
	public static String getTimeSince( String date ){
		
		String time_ago = "";
		
		try {
			
			Date start = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse( date );
			Date end = new Date();
			
			long diffInSeconds = (end.getTime() - start.getTime()) / 1000;

		    long diff[] = new long[] { 0, 0, 0, 0 };
		    /* sec */	diff[3] = (diffInSeconds >= 60 ? diffInSeconds % 60 : diffInSeconds);
		    /* min */	diff[2] = (diffInSeconds = (diffInSeconds / 60)) >= 60 ? diffInSeconds % 60 : diffInSeconds;
		    /* hours */	diff[1] = (diffInSeconds = (diffInSeconds / 60)) >= 24 ? diffInSeconds % 24 : diffInSeconds;
		    /* days */	diff[0] = (diffInSeconds = (diffInSeconds / 24));

		    // Only show days if more than 1
		    if(diff[0] > 1){
		    	time_ago += diff[0] + "d";
		    }
		    // Only show hours if > 1
		    if(diff[1] >= 1){
		    	time_ago += diff[1] + "h";
		    }
		    // Only show minutes if > 1 and less than 60
		    if(diff[2] > 1 && diff[2] < 60){
		    	time_ago += diff[2] + "m";
		    }
		    if(!time_ago.isEmpty()){
		    	time_ago += " ago";
		    }
		    if( diff[0] == 0 && diff[1] == 0 && diff[2] <= 1){
		    	time_ago = "just now";
		    }
			
		} catch (ParseException e) {
			e.printStackTrace();
			return "";
		}
		return time_ago;
	}
	
	
	/**
	 * Returns a timestring format until a date
	 * @return
	 */
	public static String getTimeUntil( String date ){
		
		String time_ago = "";
		
		try {
			
			Date start = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse( date );
			Date end = new Date();
			
			long diffInSeconds = (start.getTime() - end.getTime()) / 1000;

		    long diff[] = new long[] { 0, 0, 0, 0 };
		    /* sec */	diff[3] = (diffInSeconds >= 60 ? diffInSeconds % 60 : diffInSeconds);
		    /* min */	diff[2] = (diffInSeconds = (diffInSeconds / 60)) >= 60 ? diffInSeconds % 60 : diffInSeconds;
		    /* hours */	diff[1] = (diffInSeconds = (diffInSeconds / 60)) >= 24 ? diffInSeconds % 24 : diffInSeconds;
		    /* days */	diff[0] = (diffInSeconds = (diffInSeconds / 24));

		    // Only show days if more than 1
		    if(diff[0] > 1){
		    	time_ago += diff[0] + "d";
		    }
		    // Only show hours if > 1
		    if(diff[1] >= 1){
		    	time_ago += diff[1] + "h";
		    }
		    // Only show minutes if > 1 and less than 60
		    if(diff[2] > 1 && diff[2] < 60){
		    	time_ago += diff[2] + "m";
		    }
		    if( diff[0] == 0 && diff[1] == 0 && diff[2] <= 1){
		    	time_ago = "now";
		    }
			
		} catch (ParseException e) {
			e.printStackTrace();
			return "";
		}
		return time_ago;
	}
}
