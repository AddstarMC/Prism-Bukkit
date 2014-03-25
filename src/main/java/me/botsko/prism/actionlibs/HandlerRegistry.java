package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.plugin.Plugin;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.BlockShiftAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.EntityTravelAction;
import me.botsko.prism.actions.GenericAction;
import me.botsko.prism.actions.GrowAction;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.actions.PrismRollbackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.actions.UseAction;
import me.botsko.prism.actions.VehicleAction;
import me.botsko.prism.exceptions.InvalidActionException;

public class HandlerRegistry<H> {

    /**
	 * 
	 */
    private final HashMap<String, Class<? extends Handler>> registeredHandlers = new HashMap<String, Class<? extends Handler>>();

    /**
	 * 
	 */
    public HandlerRegistry() {
        registerPrismDefaultHandlers();
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     * 
     * @param handlerClass
     */
    protected void registerHandler(Class<? extends Handler> handlerClass) {
        final String[] names = handlerClass.getName().split( "\\." );
        if( names.length > 0 ) {
            registeredHandlers.put( names[names.length - 1], handlerClass );
        }
    }

    /**
     * Register a new action type for event recording, lookups, etc.
     * 
     * @param apiPlugin
     * @param handlerClass
     * @throws InvalidActionException
     */
    public void registerCustomHandler(Plugin apiPlugin, Class<? extends Handler> handlerClass)
            throws InvalidActionException {
        // Is plugin allowed?
        @SuppressWarnings("unchecked")
        final ArrayList<String> allowedPlugins = (ArrayList<String>) Prism.config
                .getList( "prism.tracking.api.allowed-plugins" );
        if( !allowedPlugins.contains( apiPlugin.getName() ) ) { throw new InvalidActionException(
                "Registering action type not allowed. Plugin '" + apiPlugin.getName()
                        + "' is not in list of allowed plugins." ); }
        final String[] names = handlerClass.getName().split( "\\." );
        if( names.length > 0 ) {
            registeredHandlers.put( names[names.length - 1], handlerClass );
        }
    }

    /**
     * 
     * @param name
     * @return
     */
    public Handler getHandler(String name) {
        if( name != null ) {
            if( registeredHandlers.containsKey( name ) ) {
                try {
                    final Class<? extends Handler> handlerClass = registeredHandlers.get( name );
                    return new HandlerFactory<Handler>( handlerClass ).create();
                } catch ( final InstantiationException e ) {
                    e.printStackTrace();
                } catch ( final IllegalAccessException e ) {
                    e.printStackTrace();
                }
            }
        }
        return new GenericAction();
    }

    /**
	 * 
	 */
    private void registerPrismDefaultHandlers() {

        registerHandler( GenericAction.class );
        registerHandler( BlockAction.class );
        registerHandler( BlockChangeAction.class );
        registerHandler( BlockShiftAction.class );
        registerHandler( EntityAction.class );
        registerHandler( EntityTravelAction.class );
        registerHandler( GrowAction.class );
        registerHandler( HangingItemAction.class );
        registerHandler( ItemStackAction.class );
        registerHandler( PlayerAction.class );
        registerHandler( PlayerDeathAction.class );
        registerHandler( PrismProcessAction.class );
        registerHandler( PrismRollbackAction.class );
        registerHandler( SignAction.class );
        registerHandler( UseAction.class );
        registerHandler( VehicleAction.class );

    }
}