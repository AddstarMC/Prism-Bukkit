package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

import java.util.regex.Pattern;

public class EntityParameter extends SimplePrismParameterHandler {

    /**
	 * 
	 */
    public EntityParameter() {
        super( "Entity", Pattern.compile( "[~|!]?[\\w,]+" ), "e" );
    }

    /**
	 * 
	 */
    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {
        final String[] entityNames = input.split( "," );
        if( entityNames.length > 0 ) {
            for ( final String entityName : entityNames ) {
                MatchRule match = MatchRule.INCLUDE;
                if( entityName.startsWith( "!" ) ) {
                    match = MatchRule.EXCLUDE;
                }
                query.addEntity( entityName.replace( "!", "" ), match );
            }
        }
    }
}