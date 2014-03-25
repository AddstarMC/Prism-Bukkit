package me.botsko.prism.parameters;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.utils.DateUtil;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;

import java.util.regex.Pattern;

public class SinceParameter extends SimplePrismParameterHandler {

    /**
	 * 
	 */
    public SinceParameter() {
        super( "Since", Pattern.compile( "[\\w]+" ), "t", "since" );
    }

    /**
	 * 
	 */
    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {
        if( input.equalsIgnoreCase( "none" ) ) {
            query.setIgnoreTime( true );
        } else {
            final Long date = DateUtil.translateTimeStringToDate( input );
            if( date != null ) {
                query.setSinceTime( date );
            } else {
                throw new IllegalArgumentException(
                        "Date/time for 'since' parameter value not recognized. Try /pr ? for help" );
            }
        }
    }

    /**
	 * 
	 */
    @Override
    public void defaultTo(QueryParameters query, CommandSender sender) {

        if( query.getProcessType().equals( PrismProcessType.DELETE ) )
            return;

        if( !query.getFoundArgs().contains( "before" ) && !query.getFoundArgs().contains( "since" ) ) {

            final FileConfiguration config = Bukkit.getPluginManager().getPlugin( "Prism" ).getConfig();

            Long date = DateUtil.translateTimeStringToDate( config.getString( "prism.queries.default-time-since" ) );
            if( date == 0 ) {
                Prism.log( "Error - date range configuration for prism.time-since is not valid" );
                date = DateUtil.translateTimeStringToDate( "3d" );
            }
            query.setSinceTime( date );
            query.addDefaultUsed( "t:" + config.getString( "prism.queries.default-time-since" ) );
        }
    }
}