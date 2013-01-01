package me.botsko.prism.utils;

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
}