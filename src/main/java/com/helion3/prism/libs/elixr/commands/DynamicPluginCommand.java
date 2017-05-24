package com.helion3.prism.libs.elixr.commands;

/*
* WorldEdit
* Copyright (C) 2011 sk89q <http://www.sk89q.com> and contributors
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginIdentifiableCommand;
import org.bukkit.plugin.Plugin;

import com.helion3.prism.libs.elixr.TypeUtils;
import com.helion3.prism.libs.elixr.commands.arguments.Argument;
import com.helion3.prism.libs.elixr.commands.arguments.Flag;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
* @author zml2008
* modified by viveleroi
*/
public class DynamicPluginCommand extends org.bukkit.command.Command implements PluginIdentifiableCommand {

    protected final Command commandInfo;
    protected final CommandManager registeredWith;
    protected final Plugin owningPlugin;
    protected final Method method;
    protected String[] permissions = new String[0];
    protected Map<String,DynamicPluginCommand> children = new HashMap<String,DynamicPluginCommand>();
    protected Map<String,DynamicPluginCommand> aliases = new HashMap<String,DynamicPluginCommand>();
    protected String[] args;
    protected Argument[] arguments;
    protected Flag[] flags;
    
    /**
     * 
     * @param commandInfo
     * @param registeredWith
     * @param plugin
     */
    public DynamicPluginCommand( Command commandInfo, CommandManager registeredWith, Method method, Plugin plugin ){
         super(commandInfo.aliases()[0], commandInfo.desc(), commandInfo.desc(), Arrays.asList(commandInfo.aliases()));
         this.commandInfo = commandInfo;
         this.owningPlugin = plugin;
         this.registeredWith = registeredWith;
         this.method = method;
    }
    
    /**
     * Bukkit commands pass along the execution to the owning plugin
     */
    @Override
    public boolean execute(CommandSender sender, String label, String[] args){
        this.args = args;
        return owningPlugin.onCommand(sender, this, label, args);
    }
    
    /**
     * Follows the chain of children and returns the final method to execute
     * @param sender
     * @param args
     * @return
     */
    public DynamicPluginCommand getFinalExecutionMethod(CommandSender sender, String[] args){
        this.args = args;
        if( args.length > 0 && children.containsKey( args[0] ) ){
          return children.get( args[0] ).getFinalExecutionMethod( sender, Arrays.copyOfRange(args, 1, args.length) );
        }
        else if( args.length > 0 && aliases.containsKey( args[0] ) ){
            return aliases.get( args[0] ).getFinalExecutionMethod( sender, Arrays.copyOfRange(args, 1, args.length) );
          }
        return this;
    }
    
    /**
     * 
     * @return
     */
    public String[] getArgs(){
        return args;
    }
    
    /**
     * Returns command info, described by original annotations
     * @return
     */
    public Command getInfo(){
        return commandInfo;
    }
    
    /**
     * Returns static method to be executed
     * @return
     */
    public Method getMethod(){
       return method;
    }
    
    /**
     * Set child commands
     * @param children
     */
    public void setChildren( List<DynamicPluginCommand> children ){
        for( DynamicPluginCommand command : children ){
            this.children.put( command.getName(), command );
           for( String alias : command.getAliases() ){
               this.aliases.put( alias, command );
           }
        }
    }
    
    /**
     * 
     * @return
     */
    public Map<String,DynamicPluginCommand> getChildren(){
        return children;
    }
    
    /**
     * 
     * @param arguments
     */
    public void setArguments( Argument[] arguments ){
        this.arguments = arguments;
    }
    
    /**
     * 
     * @return
     */
    public Argument[] getArguments(){
        return arguments;
    }
    
    /**
     * 
     * @param flags
     */
    public void setFlags( Flag[] flags ){
        this.flags = flags;
    }
    
    /**
     * 
     * @return
     */
    public Flag[] getFlags(){
        return flags;
    }
    
    /**
     * Set permissions levels
     * @param permissions
     */
    public void setPermissions(String[] permissions) {
         this.permissions = permissions;
         if( permissions != null ){
             super.setPermission(TypeUtils.join(permissions, ";"));
         }
    }
    
    /**
     * Returns permissions
     * @return
     */
    public String[] getPermissions() {
         return permissions;
    }
    
    /**
     * Returns owning plugin
     */
    public Plugin getPlugin() {
         return owningPlugin;
    }
    
    /**
     * 
     */
    @Override
    public boolean testPermissionSilent(CommandSender sender) {
         if (permissions == null || permissions.length == 0){
             return true;
         }
         try {
             for (String permission : permissions){
                 if (registeredWith.hasPermission(sender, permission)) {
                     return true;
                 }
             }
             return false;
         } catch (Throwable ignore){
         }
         return super.testPermissionSilent(sender);
    }
}