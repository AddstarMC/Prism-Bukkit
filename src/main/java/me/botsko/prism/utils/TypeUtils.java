package me.botsko.prism.utils;

import java.text.DecimalFormat;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.ChatColor;

public class TypeUtils {

	/**
	 * Is the string numeric
	 * 
	 * @param str
	 * @return
	 */
	public static boolean isNumeric(String str) {
		try {
			Long.parseLong(str);
		}
		catch (NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static float formatDouble(double val) {
		return Float.parseFloat(new DecimalFormat("#.##").format(val));
	}

	/**
	 * Replaces string template placeholders with values in a Hashtable. Text should
	 * be formatted with %(key) type placeholders.
	 * 
	 * @param key
	 * @param replacer
	 * @return
	 */
	public static String getStringFromTemplate(String msg, Hashtable<String, String> replacer) {
		if (msg != null && !replacer.isEmpty()) {
			for (Entry<String, String> entry : replacer.entrySet()) {
				msg = msg.replace("%(" + entry.getKey() + ")", entry.getValue());
			}
		}
		return msg;
	}

	/**
	 * Converts colors place-holders.
	 * 
	 * @param text
	 * @return
	 */
	public static String colorize(String text) {
		return ChatColor.translateAlternateColorCodes('&', text);
	}

	/**
	 * Strips all text format codes - from colors codes like &2 to text format codes
	 * like &k
	 * 
	 * @param text
	 * @return
	 */
	public static String stripTextFormatCodes(String text) {
		return ChatColor.stripColor(text.replaceAll("(&+([a-z0-9A-Z])+)", ""));
	}

	/**
	 * Joins an arraylist together by a delimiter
	 * 
	 * @param s
	 * @param delimiter
	 * @return
	 */
	public static String join(List<String> s, String delimiter) {
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
	 * Method to join array elements of type string
	 * 
	 * @author Hendrik Will, imwill.com, bug fixes by viveleroi
	 * @param inputArray Array which contains strings
	 * @param glueString String between each array element
	 * @return String containing all array elements separated by glue string
	 */
	public static String join(String[] inputArray, String glueString) {
		String output = "";
		if (inputArray.length > 0) {
			StringBuilder sb = new StringBuilder();
			if (!inputArray[0].isEmpty()) {
				sb.append(inputArray[0]);
			}
			for (int i = 1; i < inputArray.length; i++) {
				if (!inputArray[i].isEmpty()) {
					if (sb.length() > 0) {
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
	 * 
	 * @param s
	 * @return
	 */
	public static String strToUpper(String s) {
		return s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();
	}

	/**
	 * Java implementation of preg_match_all by https://github.com/raimonbosch
	 * 
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

	/**
	 * 
	 * @param str
	 * @param findStr
	 * @return
	 */
	public static int subStrOccurences(String str, String findStr) {
		int lastIndex = 0, count = 0;
		while (lastIndex != -1) {
			lastIndex = str.indexOf(findStr, lastIndex);
			if (lastIndex != -1) {
				count++;
				lastIndex += findStr.length();
			}
		}
		return count;
	}

	/**
	 * 
	 * @param str
	 * @param desiredLength
	 * @return
	 */
	public static String padStringRight(String str, int desiredLength) {
		if (str.length() >= desiredLength)
			return str.substring(0, desiredLength);
		StringBuilder sb = new StringBuilder();
		int rest = desiredLength - str.length();
		for (int i = 1; i < rest; i++) {
			sb.append(" ");
		}
		return str + sb.toString();
	}
}
