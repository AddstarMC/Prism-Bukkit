package me.botsko.prism.commandlibs;


public final class SubCommand {
	private final String name;
	private boolean allow_console = false;
	private int minArgs = 0;
	private SubHandler handler = null;
	private String description;
	private String usage = null;
	private String perm_node;
	
	/**
	 * 
	 * @param name
	 * @param permission
	 */
	public SubCommand(String name, String permission) {
		this.name = name;
		this.perm_node = permission;
	}

	
	/**
	 * 
	 * @param name
	 * @param permission
	 * @param handler
	 */
	public SubCommand(String name, String permission, SubHandler handler) {
		this(name, permission);
		this.handler = handler;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public SubCommand allowConsole() {
		this.allow_console = true;
		return this;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public boolean isConsoleAllowed() {
		return this.allow_console;
	}

	
	/**
	 * 
	 * @return
	 */
	public int getMinArgs() {
		return minArgs;
	}

	
	/**
	 * 
	 * @param minArgs
	 * @return
	 */
	public SubCommand setMinArgs(int minArgs) {
		this.minArgs = minArgs;
		return this;
	}

	
	/**
	 * 
	 * @return
	 */
	public SubHandler getHandler() {
		return handler;
	}

	
	/**
	 * 
	 * @param handler
	 * @return
	 */
	public SubCommand setHandler(SubHandler handler) {
		this.handler = handler;
		return this;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getPermNode(){
		return perm_node;
	}
	
	
	/**
	 * 
	 * @param node
	 */
	public void setPermNode( String node ){
		perm_node = node;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getName() {
		return name;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getUsage() {
		return this.usage;
	}
	
	
	/**
	 * 
	 * @param usage
	 * @return
	 */
	public SubCommand setUsage(String usage) {
		this.usage = usage;
		return this;
	}

	
	/**
	 * 
	 * @return
	 */
	public String getDescription() {
		return description;
	}

	
	/**
	 * 
	 * @param description
	 * @return
	 */
	public SubCommand setDescription(String description) {
		this.description = description;
		return this;
	}
	
}

