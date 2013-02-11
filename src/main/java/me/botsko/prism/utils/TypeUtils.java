package me.botsko.prism.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
	
	
	
	/**
	* Method to join array elements of type string
	* @author Hendrik Will, imwill.com, bug fixes by viveleroi
	* @param inputArray Array which contains strings
	* @param glueString String between each array element
	* @return String containing all array elements separated by glue string
	*/
	public static String implode(String[] inputArray, String glueString) {

		/** Output variable */
		String output = "";
	
		if (inputArray.length > 0) {
			StringBuilder sb = new StringBuilder();
			if(!inputArray[0].isEmpty()){
				sb.append(inputArray[0]);
			}
			for (int i=1; i<inputArray.length; i++) {
				if(!inputArray[i].isEmpty()){
					if(sb.length() > 0){
						sb.append(glueString);
					}
					sb.append(inputArray[i]);
				}
			}
	
			output = sb.toString();
		}
	
		return output;
	}
	
	
	
	/**
	 * Java implementation of preg_match_all by https://github.com/raimonbosch
	 * @param p
	 * @param subject
	 * @return
	 */
	public static String[] preg_match_all(Pattern p, String subject) {
		Matcher m = p.matcher(subject);
		StringBuilder out = new StringBuilder();
		boolean split = false;
		while (m.find()) {
			out.append(m.group());
			out.append("~");
			split = true;
		}
		return (split) ? out.toString().split("~") : new String[0];
	}
}