package me.botsko.prism.parameters;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

import java.util.regex.Pattern;

public class IdParameter extends SimplePrismParameterHandler {

    /**
	 * 
	 */
    public IdParameter() {
        super( "ID", Pattern.compile( "[\\d,]+" ), "id" );
    }

    /**
	 * 
	 */
    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {

        if( !TypeUtils.isNumeric( input ) ) { throw new IllegalArgumentException(
                "ID must be a number. Use /prism ? for help." ); }
        query.setId( Integer.parseInt( input ) );
    }
}