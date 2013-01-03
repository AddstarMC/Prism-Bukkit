package me.botsko.prism.utils;

import java.util.ArrayList;
import java.util.Iterator;

public class TypeUtils {
	
	
	/**
     * 
     * @param str
     * @return
     */
	public static boolean isNumeric(String str){  
		try{  
			Integer.parseInt(str);
		}
		catch(NumberFormatException nfe){  
			return false;
		}
		return true;
	}
	
	
	/**
	 * 
	 * @param s
	 * @param delimiter
	 * @return
	 */
	public static String join(ArrayList<String> s, String delimiter) {
		StringBuffer buffer = new StringBuffer();
		Iterator<?> iter = s.iterator();
		while (iter.hasNext()) {
			buffer.append(iter.next());
			if (iter.hasNext())
				buffer.append(delimiter);
		}
		return buffer.toString();
	}
	
	
	/**
	 * 
	 * @param s
	 * @return
	 */
	public static String strToUpper(String s){
		return s.substring(0,1).toUpperCase()+s.substring(1).toLowerCase();
	}
}