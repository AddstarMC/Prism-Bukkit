package me.botsko.prism.utils;

import me.botsko.prism.Prism;
import net.md_5.bungee.api.ChatColor;

import java.awt.Color;
import java.text.DecimalFormat;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("unused")
public class TypeUtils {
    private static final String HEX_REGEX = "#([A-Fa-f0-9]{6})";
    private static final Pattern HEX_PATTERN = Pattern.compile(HEX_REGEX);

    /**
     * Is the string numeric.
     *
     * @param str String
     * @return boolean
     */
    public static boolean isNumeric(String str) {
        try {
            Long.parseLong(str);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    /**
     * Format a double to a float.
     *
     * @param val double
     * @return Float
     */
    public static float formatDouble(double val) {
        return Float.parseFloat(new DecimalFormat("#.##").format(val));
    }

    /**
     * Replaces string template placeholders with values in a Hashtable. Text should
     * be formatted with %(key) type placeholders.
     *
     * @param msg      the message
     * @param replacer the replacement Map
     * @return a String
     */
    public static String getStringFromTemplate(String msg, Map<String, String> replacer) {
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
     * @param text String
     * @return String
     */
    public static String colorize(String text) {
        text = parseRgbColours(text);
        return ChatColor.translateAlternateColorCodes('&', text);
    }

    /**
     * Parse RBG to Color.
     *
     * @param input String
     * @return String
     */
    public static String parseRgbColours(final String input) {
        String out = input;
        Matcher matcher = HEX_PATTERN.matcher(out);
        while (matcher.find()) {
            int groups = matcher.groupCount();
            for (int i = 0; i < groups; i++) {
                String hex = matcher.group(i);
                Color color;
                try {
                    color = Color.decode(hex);
                    out = out.replace(hex, net.md_5.bungee.api.ChatColor.of(color).toString());
                } catch (NumberFormatException e) {
                    out = out.replace(hex, "");
                    Prism.log("Invalid hex code removed: " + hex + " from " + input);
                }
            }
            matcher = HEX_PATTERN.matcher(out);
        }
        return out;
    }

    /**
     * Get Color.
     *
     * @param hex String.
     * @return ChatColor.
     */
    public static ChatColor from(String hex) {
        if (hex.length() == 2 && hex.startsWith("&")) {
            return ChatColor.getByChar(hex.charAt(1));
        }
        if (hex.length() != 7 && !hex.startsWith("#")) {
            Prism.log("Could not decode:" + hex);
            return ChatColor.WHITE;
        }
        try {
            return ChatColor.of(hex);
        } catch (NumberFormatException e) {
            Prism.log("Could not decode:" + hex + " Exception:" + e.getLocalizedMessage());
            return ChatColor.WHITE;
        }


    }

    /**
     * Strips all text format codes - from colors codes like &2 to text format codes
     * like &k.
     *
     * @param text String
     * @return String
     */
    public static String stripTextFormatCodes(String text) {
        return ChatColor.stripColor(text.replaceAll(HEX_REGEX, "").replaceAll("(&+([a-z0-9A-Z])+)", ""));
    }

    /**
     * Joins an arraylist together by a delimiter.
     *
     * @param s         Collection
     * @param delimiter String
     * @return String
     */
    public static String join(Collection<String> s, String delimiter) {
        StringBuilder buffer = new StringBuilder();
        Iterator<?> iter = s.iterator();
        while (iter.hasNext()) {
            buffer.append(iter.next());
            if (iter.hasNext()) {
                buffer.append(delimiter);
            }
        }
        return buffer.toString();
    }

    /**
     * Method to join array elements of type string.
     *
     * @param inputArray Array which contains strings
     * @param glueString String between each array element
     * @return String containing all array elements separated by glue string
     * @author Hendrik Will, imwill.com, bug fixes by viveleroi
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
     * Upper case.
     *
     * @param s String
     * @return String
     */
    public static String strToUpper(String s) {
        return s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();
    }

    /**
     * Java implementation of preg_match_all by https://github.com/raimonbosch
     *
     * @param p       Pattern
     * @param subject CharSequence
     * @return String[]
     */
    public static String[] preg_match_all(Pattern p, CharSequence subject) {
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
     * Position of occurance.
     *
     * @param str     String
     * @param findStr String
     * @return int
     */
    public static int subStrOccurences(String str, String findStr) {
        int lastIndex = 0;
        int count = 0;
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
     * Pad a String.
     *
     * @param str           String
     * @param desiredLength int
     * @return String
     */
    public static String padStringRight(String str, int desiredLength) {
        if (str.length() >= desiredLength) {
            return str.substring(0, desiredLength);
        }
        StringBuilder sb = new StringBuilder();
        int rest = desiredLength - str.length();
        for (int i = 1; i < rest; i++) {
            sb.append(" ");
        }
        return str + sb.toString();
    }
}
