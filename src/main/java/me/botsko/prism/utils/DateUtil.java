package me.botsko.prism.utils;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import me.botsko.elixr.TypeUtils;

public class DateUtil {

    /**
     * 
     * @return
     */
    public static Long translateTimeStringToDate(String arg_value) {

        Long dateFrom = 0L;

        final Pattern p = Pattern.compile( "([0-9]+)(s|h|m|d|w)" );
        final Calendar cal = Calendar.getInstance();

        final String[] matches = TypeUtils.preg_match_all( p, arg_value );
        if( matches.length > 0 ) {
            for ( final String match : matches ) {

                final Matcher m = p.matcher( match );
                if( m.matches() ) {

                    if( m.groupCount() == 2 ) {

                        final int tfValue = Integer.parseInt( m.group( 1 ) );
                        final String tfFormat = m.group( 2 );

                        if( tfFormat.equals( "w" ) ) {
                            cal.add( Calendar.WEEK_OF_YEAR, -1 * tfValue );
                        } else if( tfFormat.equals( "d" ) ) {
                            cal.add( Calendar.DAY_OF_MONTH, -1 * tfValue );
                        } else if( tfFormat.equals( "h" ) ) {
                            cal.add( Calendar.HOUR, -1 * tfValue );
                        } else if( tfFormat.equals( "m" ) ) {
                            cal.add( Calendar.MINUTE, -1 * tfValue );
                        } else if( tfFormat.equals( "s" ) ) {
                            cal.add( Calendar.SECOND, -1 * tfValue );
                        } else {
                            return null;
                        }
                    }
                }
            }
            dateFrom = cal.getTime().getTime();
        }

        return dateFrom;

    }
}
