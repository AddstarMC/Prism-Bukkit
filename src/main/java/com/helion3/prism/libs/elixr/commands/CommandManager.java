package com.helion3.prism.libs.elixr.commands;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandMap;
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

import com.helion3.prism.libs.elixr.ReflectionUtil;
import com.helion3.prism.libs.elixr.TypeUtils;
import com.helion3.prism.libs.elixr.commands.arguments.Argument;
import com.helion3.prism.libs.elixr.commands.arguments.ArgumentValidator;
import com.helion3.prism.libs.elixr.commands.arguments.Flag;
import com.helion3.prism.libs.elixr.commands.exceptions.CommandArgumentException;
import com.helion3.prism.libs.elixr.commands.exceptions.CommandPermissionException;
import com.helion3.prism.libs.elixr.commands.exceptions.IllegalCommandSenderException;

public class CommandManager {
    
    // Retain plugin as owner/CommandExecutor
    private final Plugin plugin;
    
    // Cache the root commands we own, for easy usage lookup
    private final Map<String,DynamicPluginCommand> ownedCommands = new HashMap<String,DynamicPluginCommand>();

    /**
     * 
     * @param plugin
     */
    public CommandManager( Plugin plugin ){
        this.plugin = plugin;
    }
 
    /**
     * Register root level command class (and all children)
     * @param cls
     * @throws Exception
     */
    public void register(Class<?> cls) throws Exception {

        List<DynamicPluginCommand> commands = registerDynamicCommand(cls);
        
        // Register complete command with Bukkit
        CommandMap commandMap = getCommandMap();
        for( DynamicPluginCommand command : commands ){
            ownedCommands.put( command.getName(), command );
            commandMap.register(plugin.getDescription().getName(), command);
        }
    }

    /**
     * Permission check for the CommandSender
     * @param sender
     * @param permission
     * @return
     */
    public boolean hasPermission( CommandSender sender, String permission ){
        return sender instanceof ConsoleCommandSender || sender.hasPermission(permission);
    }
    
    /**
     * Execute a command
     * @param sender
     * @param cmd
     * @param args
     * @throws IllegalCommandSenderException 
     * @throws InvocationTargetException 
     * @throws IllegalAccessException 
     * @throws IllegalArgumentException 
     * @throws CommandPermissionException 
     * @throws CommandArgumentException 
     * @throws Exception
     */
    public void execute( CommandSender sender, org.bukkit.command.Command cmd, String[] args ) throws IllegalCommandSenderException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, CommandPermissionException, CommandArgumentException{
        
        if( !(cmd instanceof DynamicPluginCommand) ){
            throw new IllegalArgumentException("Command is not of type DynamicPluginCommand");
        }
        
        // Relay execution to any children, if any
        DynamicPluginCommand baseCommand = (DynamicPluginCommand) cmd;
        DynamicPluginCommand command = baseCommand.getFinalExecutionMethod( sender, args );

        // Sender check: Console
        if( sender instanceof ConsoleCommandSender && ( !command.getInfo().allowsConsole() || command.getInfo().playerRequired() ) ){
            throw new IllegalCommandSenderException("User 'console' is not allowed for command " + command.getName());
        }
        
        // Sender check: Must be player
        if( !(sender instanceof Player) && command.getInfo().playerRequired() ){
            throw new IllegalCommandSenderException("User must be a player for command " + command.getName());
        }
        
        // Permissions check
        for( String permission : command.getPermissions() ){
            if( !hasPermission( sender, permission ) ){
                throw new CommandPermissionException("You do not have permission for the command " + command.getName());
            }
        }
        
        try {
        
        // raw incoming args
        String[] rawArgs = command.getArgs();
        
        // Flag checks
        Flag[] flags = command.getFlags();
        Map<String,String> foundFlags = new HashMap<String,String>();
        if( flags != null && flags.length > 0 ){
            for( Flag f : flags ){
                for( String rawArg : rawArgs ){
                    
                    if( !rawArg.startsWith("-") ) continue;
                    
                    rawArg = rawArg.replace("-","");
                    String flagKey = rawArg;
                    String flagVal = "";
                    if( rawArg.contains("=") ){
                        String flagInfo[] = rawArg.split("=");
                        flagKey = flagInfo[0];
                        if( flagInfo.length == 2 ){
                            flagVal = flagInfo[1];
                        }
                    }
                    for( String flagAlias : f.aliases() ){
                        if( flagKey.equalsIgnoreCase(flagAlias) ){
                            if( f.acceptsValue() ){
                                foundFlags.put(f.aliases()[0], flagVal );
                            } else {
                                foundFlags.put(f.aliases()[0], null );
                            }
                        }
                    }
                }
            }
        }
        
        // Arg checks
        Argument[] arguments = command.getArguments();
        Map<String,Integer> namedArguments = new HashMap<String,Integer>();
        CommandArguments methodArgs;
        if( arguments != null ){
            
            List<String> finalArgs = new LinkedList<String>();
            
            // Preprocess to find any joined args
            for( int index = 0; index < arguments.length; index++ ){
                if( index >= rawArgs.length ) break;
                Argument defined = arguments[index];
                String currentArg =  rawArgs[index];
                if( currentArg.startsWith("-") ) continue;
                // If it joins remaining arguments into one...
                if( defined.joinsRemaining() ){
                    for( int i = index+1; i < rawArgs.length; i++ ){
                        currentArg += " "+rawArgs[i];
                    }
                }
                finalArgs.add( currentArg );
            }
            
            // Iterate provided arguments and validate them
            for( int index = 0; index < arguments.length; index++ ){
                
                Argument defined = arguments[index];

                // If required but arg missing...
                if( defined.required() && (index+1) > finalArgs.size() ){
                    throw new CommandArgumentException("Too few arguments for command " + command.getName());
                }
                
                // If it's optional, but not present, use default
                if( (index+1) > finalArgs.size() ){
                    if( !defined.defaultValue().isEmpty() ){
                        finalArgs.add( defined.defaultValue() );
                    } else {
                        break;
                    }
                } else {
                
                    String rawArg = finalArgs.get( index );
                    
                    // Validate
                    if( defined.validator() != null ){
                        try {
                            ArgumentValidator validator = defined.validator().newInstance();
                            validator.validate( rawArg );  
                        } catch( InstantiationException e ){
                            e.printStackTrace();
                        }
                    }
                }
                
                namedArguments.put( defined.name(), index );
                
            }

            methodArgs = new CommandArguments(finalArgs.toArray(new String[finalArgs.size()]),namedArguments,foundFlags);
            
        } else {
            methodArgs = new CommandArguments(foundFlags);
        }
        
        // We're the one handling the command
        Method method = command.getMethod();
        if( method == null ){
            // throw exception
            return;
        }
        try {
            method.invoke( null, sender, methodArgs );
        } catch (InvocationTargetException ite) {
            if (ite.getCause() instanceof CommandArgumentException){
                throw (CommandArgumentException) ite.getCause();
            }
        }
        
        } catch(Exception e){
            e.printStackTrace();
        }


    }
    
    /**
     * Returns a hierarchical list of command usage
     * @return
     */
    public List<String> getUsages(){
        List<String> usage = new ArrayList<String>();
        for( Entry<String,DynamicPluginCommand> entry : ownedCommands.entrySet() ){
            usage.addAll( getDeepCommandUsages( entry.getValue(), "/" ) ); 
        }
        return usage;
    }
    
    /**
     * Recursive usage menu builder
     * @param command
     * @return
     */
    private List<String> getDeepCommandUsages( DynamicPluginCommand command, String prefix ){

        // Start with this command
        List<String> usage = new ArrayList<String>();
        
        String commandName = "";
        if( command.getAliases().size() > 1 ){
            commandName = "("+TypeUtils.join( command.getAliases(), "|" )+")";
        } else {
            commandName = command.getName();
        }
        
        String commandPrefix = prefix + commandName;

        // List arguments
        if( command.getInfo().arguments().length > 0 ){
            String commandArgs = new String(commandPrefix);
            for( Argument arg : command.getInfo().arguments() ){
                if( arg.required() ){
                    commandArgs += " [" + arg.name() + "]";
                } else {
                    commandArgs += " (" + arg.name() + ")";
                }
            }
            usage.add( commandArgs + " - " + command.getInfo().desc() );
        } else {
            usage.add( commandPrefix + " - " + command.getInfo().desc() );
        }
        
        // Append children
        for( Entry<String,DynamicPluginCommand> entry : command.getChildren().entrySet() ){
            usage.addAll( getDeepCommandUsages(entry.getValue(), commandPrefix+" ") );
        }
        
        return usage;
    }
    
    /**
     * Register the methods of a class.
     *
     * @param cls
     * @param parent
     * @param obj
     * @return
     * @throws Exception 
     */
    private List<DynamicPluginCommand> registerDynamicCommand(Class<?> cls) throws Exception {
        
        List<DynamicPluginCommand> commands = new ArrayList<DynamicPluginCommand>();

        // Iterate all methods in the class
        for( Method method : cls.getMethods() ){
            
            if( !method.isAnnotationPresent(Command.class) ) continue;
            if( !Modifier.isStatic(method.getModifiers()) ){
                throw new Exception("Annotated command method must be static");
            }

            // Build command
            Command cmd = method.getAnnotation(Command.class);
            DynamicPluginCommand bukkitCommand = new DynamicPluginCommand( cmd, this, method, plugin);
            
            // Parse permissions
            if( cmd.permissions().length > 0 ){
                bukkitCommand.setPermissions( cmd.permissions() );
            }
            
            // Parse arguments
            if( cmd.arguments().length > 0 ){
                bukkitCommand.setArguments(cmd.arguments());
            }
            
            // Parse flags
            if( cmd.flags().length > 0 ){
                bukkitCommand.setFlags(cmd.flags());
            }
            
            // Parse children
            if( method.isAnnotationPresent(NestedCommands.class) ){
                NestedCommands children = method.getAnnotation(NestedCommands.class);
                List<DynamicPluginCommand> childrenCommands = new ArrayList<DynamicPluginCommand>();
                for( Class<?> child : children.value() ){
                    childrenCommands.addAll( registerDynamicCommand(child) );
                }
                bukkitCommand.setChildren( childrenCommands );
            }
            
            commands.add( bukkitCommand );

        }
        
        return commands;
        
    }
    
    /**
     * 
     * @return
     * @throws Exception
     */
    private CommandMap getCommandMap() throws Exception{
        CommandMap commandMap = ReflectionUtil.getField(Bukkit.getServer().getPluginManager(), "commandMap");
        if (commandMap == null){
            throw new Exception("Invalid command map.");
        }
        return commandMap;
    }
}