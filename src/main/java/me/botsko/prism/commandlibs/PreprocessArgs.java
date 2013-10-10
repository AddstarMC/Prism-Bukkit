package me.botsko.prism.commandlibs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import me.botsko.elixr.ChunkUtils;
import me.botsko.elixr.ItemUtils;
import me.botsko.elixr.MaterialAliases;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.WorldEditBridge;
import me.botsko.prism.utils.LevenshteinDistance;

import org.bukkit.Chunk;
import org.bukkit.Location;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PreprocessArgs {

	/**
	 * @param sender
	 * @param msg
	 */
	protected static void respond(CommandSender sender, String msg) {
		if (sender != null) {
			sender.sendMessage(msg);
		} else {
			// System.out.print(msg); // @todo let this output to console, is useful for db purge debugging
		}
	}

	/**
	 * @param args
	 */
	public static QueryParameters process(Prism plugin, CommandSender sender, String[] args, PrismProcessType processType, int startAt, boolean useDefaults) {

		Player player = null;
		if (sender != null && sender instanceof Player) {
			player = (Player) sender;
		}

		QueryParameters parameters = new QueryParameters();
		ConcurrentHashMap<String, String> foundArgs = new ConcurrentHashMap<String, String>();
		if (processType.equals(PrismProcessType.LOOKUP)) {
			parameters.setLimit(plugin.getConfig().getInt("prism.queries.lookup-max-results"));
			parameters.setPerPage(plugin.getConfig().getInt("prism.queries.default-results-per-page"));
		}
		parameters.setProcessType(processType);

		if (args != null) {

			// Iterate over arguments
			for (int i = startAt; i < args.length; i++) {

				String arg = args[i];
				if (arg.isEmpty())
					continue;

				// disabling because the alt player syntax won't work
				// // Verify they're formatting like a:[val] or like -arg
				// if(!(arg.contains(":") || arg.contains("-"))){
				// respond( sender, Prism.messenger.playerError("Missing or invalid parameter value for '"+arg+"'. Use /prism ? for help.") );
				// return null;
				// }
				// if (!(arg.contains(":") || arg.substring(0,1).equals("-"))){
				// respond( sender, Prism.messenger.playerError("Misplaced colon for '"+arg+"'. Use /prism ? for help.") );
				// return null;
				// }

				// Split parameter and values
				String[] argEntry = arg.toLowerCase().split(":");
				String arg_type = argEntry[0];
				String val = arg.contains(":") ? arg.replace(argEntry[0] + ":", "") : argEntry[0];

				// Verify we have an arg we can match
				String[] possibleArgs = { "a", "r", "t", "p", "w", "b", "e", "k", "before", "since", "id", "-" };
				if (!Arrays.asList(possibleArgs).contains(arg_type) && !arg_type.startsWith("-")) {

					// We support an alternate player syntax so that people can use the tab-complete
					// feature of minecraft. Using p: prevents it.
					Player autoFillPlayer = plugin.getServer().getPlayer(arg_type);
					if (autoFillPlayer != null) {
						MatchRule match = MatchRule.INCLUDE;
						if (arg_type.startsWith("!")) {
							match = MatchRule.EXCLUDE;
						}
						parameters.addPlayerName(arg_type.replace("!", ""), match);
					} else {
						respond(sender, Prism.messenger.playerError("Unrecognized parameter '" + arg + "'. Use /prism ? for help."));
						return null;
					}
				}

				// Verify no empty val
				if (val.isEmpty()) {
					respond(sender, Prism.messenger.playerError("Can't use empty values for '" + arg + "'. Use /prism ? for help."));
					return null;
				}

				// Officially certify we found a valid argument and value!
				if (Arrays.asList(possibleArgs).contains(arg_type)) {
					foundArgs.put(arg_type, val);
					parameters.setFoundArgs(foundArgs);
				}

				// Action
				if (arg_type.equals("a")) {
					String[] actions = val.split(",");
					if (actions.length > 0) {
						for (String action : actions) {
							// Find all actions that match the action provided - whether the full name or
							// short name.
							ArrayList<ActionType> actionTypes = Prism.getActionRegistry().getActionsByShortname(action.replace("!", ""));
							if (!actionTypes.isEmpty()) {
								for (ActionType actionType : actionTypes) {

									// Ensure the action allows this process type
									if ((processType.equals(PrismProcessType.ROLLBACK) && !actionType.canRollback()) || (processType.equals(PrismProcessType.RESTORE) && !actionType.canRestore())) {
										// @todo this is important information but is too spammy with a:place, because vehicle-place doesn't support a rollback etc
										// respond( sender, Prism.messenger.playerError("Ingoring action '"+actionType.getName()+"' because it doesn't support rollbacks.") );
										continue;
									}

									// Check match type
									MatchRule match = MatchRule.INCLUDE;
									if (action.startsWith("!")) {
										match = MatchRule.EXCLUDE;
									}
									parameters.addActionType(actionType.getName(), match);
								}
							} else {
								respond(sender, Prism.messenger.playerError("Ignoring action '" + action.replace("!", "") + "' because it's unrecognized. Did you mean '" + LevenshteinDistance.getClosestAction(action) + "'? Type '/prism params' for help."));
							}
						}
						// If none were valid, we end here.
						if (parameters.getActionTypes().size() == 0) {
							return null;
						}
					}
				}

				// Player
				if (arg_type.equals("p")) {
					String[] playerNames = val.split(",");
					if (playerNames.length > 0) {
						for (String playerName : playerNames) {
							MatchRule match = MatchRule.INCLUDE;
							if (playerName.startsWith("!")) {
								match = MatchRule.EXCLUDE;
								playerName = playerName.replace("!", "");
							}
							else if (playerName.startsWith("~")) {
								match = MatchRule.PARTIAL;
								playerName = playerName.replace("~", "");
							}
							parameters.addPlayerName(playerName, match);
						}
					}
				}

				// World
				if (arg_type.equals("w")) {
					if (val.equalsIgnoreCase("current")) {
						if (player != null) {
							val = player.getWorld().getName();
						} else {
							sender.sendMessage(Prism.messenger.playerError("Can't use the current world since you're not a player. Using default world."));
							val = plugin.getServer().getWorlds().get(0).getName();
						}
					}
					parameters.setWorld(val);
				}

				// Radius
				if (arg_type.equals("r")) {
					if (TypeUtils.isNumeric(val) || (val.contains(":") && val.split(":").length >= 1 && TypeUtils.isNumeric(val.split(":")[1]))) {
						int radius;
						Location coordsLoc = null;
						if (val.contains(":")) {
							radius = Integer.parseInt(val.split(":")[1]);
							String radiusLocOrPlayer = val.split(":")[0];
							if (radiusLocOrPlayer.contains(",")) { // Cooridinates; x,y,z
								String[] coordinates = radiusLocOrPlayer.split(",");
								if (coordinates.length != 3) {
									respond(sender, Prism.messenger.playerError("Couldn't parse the coordinates '" + radiusLocOrPlayer + "'. Perhaps you have more than two commas?"));
									return null;
								}
								for (String s : coordinates) {
									if (!TypeUtils.isNumeric(s)) {
										respond(sender, Prism.messenger.playerError("The coordinate '" + s + "' is not a number."));
										return null;
									}
								}
								coordsLoc = (new Location(
										player != null ? player.getWorld() :
												(parameters.getWorld() != null ? plugin.getServer().getWorld(parameters.getWorld()) :
														plugin.getServer().getWorlds().get(0)),
										Integer.parseInt(coordinates[0]),
										Integer.parseInt(coordinates[1]),
										Integer.parseInt(coordinates[2])));

							}
							else if (plugin.getServer().getPlayer(radiusLocOrPlayer) != null) {
								player = plugin.getServer().getPlayer(radiusLocOrPlayer);
							} else {
								respond(sender, Prism.messenger.playerError("Couldn't find the player named '" + radiusLocOrPlayer + "'. Perhaps they are not online or you misspelled their name?"));
								return null;
							}
						} else {
							radius = Integer.parseInt(val);
						}
						if (radius <= 0) {
							respond(sender, Prism.messenger.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help."));
							return null;
						}
						// Does the radius exceed the configured max?
						if (parameters.getProcessType().equals(PrismProcessType.LOOKUP) && radius > plugin.getConfig().getInt("prism.queries.max-lookup-radius")) {
							// If player does not have permission to override the max
							if (player != null && !player.hasPermission("prism.override-max-lookup-radius")) {
								radius = plugin.getConfig().getInt("prism.queries.max-lookup-radius");
								respond(sender, Prism.messenger.playerError("Forcing radius to " + radius + " as allowed by config."));
							}
						}
						if (!parameters.getProcessType().equals(PrismProcessType.LOOKUP) && radius > plugin.getConfig().getInt("prism.queries.max-applier-radius")) {
							// If player does not have permission to override the max
							if (player != null && !player.hasPermission("prism.override-max-applier-radius")) {
								radius = plugin.getConfig().getInt("prism.queries.max-applier-radius");
								respond(sender, Prism.messenger.playerError("Forcing radius to " + radius + " as allowed by config."));
							}
						}
						if (radius > 0) {
							parameters.setRadius(radius);
							if (coordsLoc != null) {
								parameters.setMinMaxVectorsFromPlayerLocation(coordsLoc); // We need to set this *after* the radius has been set or it won't work.
							} else {
								if (player != null) {
									parameters.setMinMaxVectorsFromPlayerLocation(player.getLocation());
								}
							}
						}
					} else {

						// User wants an area inside of a worldedit selection
						if (val.equals("we")) {

							if (plugin.plugin_worldEdit == null) {
								respond(sender, Prism.messenger.playerError("This feature is disabled because Prism couldn't find WorldEdit."));
								return null;
							} else {

								// Load a selection from world edit as our area.
								if (player != null) {
									parameters = WorldEditBridge.getSelectedArea(plugin, player, parameters);
								}
							}
						}

						// Confine to the chunk
						else if (val.equals("c") || val.equals("chunk")) {

							if (player == null) {
								respond(sender, Prism.messenger.playerError("Chunks cannot be used as a radius without a player."));
								return null;
							}

							Chunk ch = player.getLocation().getChunk();
							parameters.setWorld(ch.getWorld().getName());
							parameters.setMinLocation(ChunkUtils.getChunkMinVector(ch));
							parameters.setMaxLocation(ChunkUtils.getChunkMaxVector(ch));

						}

						// User wants no radius, but contained within the current world
						else if (val.equals("world")) {
							// Do they have permission to override the global lookup radius
							if (parameters.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-lookup-radius")) {
								respond(sender, Prism.messenger.playerError("You do not have permission to override the max radius."));
								return null;
							}
							// Do they have permission to override the global applier radius
							if (!parameters.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-applier-radius")) {
								respond(sender, Prism.messenger.playerError("You do not have permission to override the max radius."));
								return null;
							}
							// Use the world defined in the w: param
							if (parameters.getWorld() != null) {
								val = parameters.getWorld();
							}
							// Use the current world
							else if (player != null) {
								val = player.getWorld().getName();
							}
							// Use the default world
							else {
								sender.sendMessage(Prism.messenger.playerError("Can't use the current world since you're not a player. Using default world."));
								val = plugin.getServer().getWorlds().get(0).getName();
							}
							parameters.setWorld(val);
							parameters.setAllowNoRadius(true);
						}

						// User has asked for a global radius
						else if (val.equals("global")) {
							// Do they have permission to override the global lookup radius
							if (parameters.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-lookup-radius")) {
								respond(sender, Prism.messenger.playerError("You do not have permission to override the max radius."));
								return null;
							}
							// Do they have permission to override the global applier radius
							if (!parameters.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-applier-radius")) {
								respond(sender, Prism.messenger.playerError("You do not have permission to override the max radius."));
								return null;
							}
							// Either they have permission or player is null
							parameters.setWorld(null);
							parameters.setAllowNoRadius(true);

						} else {
							respond(sender, Prism.messenger.playerError("Radius is invalid. There's a bunch of choice, so use /prism actions for assistance."));
							return null;
						}
					}
				}

				// Entity
				if (arg_type.equals("e")) {
					String[] entityNames = val.split(",");
					if (entityNames.length > 0) {
						for (String entityName : entityNames) {
							MatchRule match = MatchRule.INCLUDE;
							if (entityName.startsWith("!")) {
								match = MatchRule.EXCLUDE;
							}
							parameters.addEntity(entityName.replace("!", ""), match);
						}
					}
				}

				// Block
				if (arg_type.equals("b")) {

					String[] blocks = val.split(",");

					if (blocks.length > 0) {
						for (String b : blocks) {

							// if user provided id:subid
							if (b.contains(":") && b.length() >= 3) {
								String[] ids = b.split(":");
								if (ids.length == 2 && TypeUtils.isNumeric(ids[0]) && TypeUtils.isNumeric(ids[1])) {
									parameters.addBlockFilter(Integer.parseInt(ids[0]), Byte.parseByte(ids[1]));
								} else {
									respond(sender, Prism.messenger.playerError("Invalid block filter '" + val + "'. Use /prism ? [command] for help."));
									return null;
								}
							} else {

								// It's id without a subid
								if (TypeUtils.isNumeric(b)) {
									parameters.addBlockFilter(Integer.parseInt(b), (byte) 0);
								} else {

									// Lookup the item name, get the ids
									MaterialAliases items = plugin.getItems();
									ArrayList<int[]> itemIds = items.getIdsByAlias(b);
									if (itemIds.size() > 0) {
										for (int[] ids : itemIds) {
											if (ids.length == 2) {
												// If we really care about the sub id because it's a whole different item
												if (ItemUtils.dataValueUsedForSubitems(ids[0])) {
													parameters.addBlockFilter(ids[0], (byte) ids[1]);
												} else {
													parameters.addBlockFilter(ids[0], (byte) 0);
												}
											}
										}
									}
								}
							}
						}
					}
				}

				// Time
				if (arg_type.equals("before")) {
					Long date = translateTimeStringToDate(plugin, sender, val);
					if (date != null) {
						parameters.setBeforeTime(date);
					} else {
						return null;
					}
				}
				if (arg_type.equals("since") || arg_type.equals("t")) {
					if (val.equalsIgnoreCase("none")) {
						parameters.setIgnoreTime(true);
					} else {
						Long date = translateTimeStringToDate(plugin, sender, val);
						if (date != null) {
							parameters.setSinceTime(date);
						} else {
							return null;
						}
					}
				}

				// Keyword
				if (arg_type.equals("k")) {
					parameters.setKeyword(val);
				}

				// ID
				if (arg_type.equals("id")) {

					if (TypeUtils.isNumeric(val)) {

					} else {
						respond(sender, Prism.messenger.playerError("ID must be a number. Use /prism ? for help."));
						return null;
					}
					parameters.setId(Integer.parseInt(val));
				}

				// Special Flags
				if (arg_type.startsWith("-")) {
					try {
						String[] flagComponents = val.substring(1).split("=");
						Flag flag = Flag.valueOf(flagComponents[0].replace("-", "_").toUpperCase());
						if (!(parameters.hasFlag(flag))) {

							parameters.addFlag(flag);

							// Flag has a value
							if (flagComponents.length > 1) {
								if (flag.equals(Flag.PER_PAGE)) {
									if (TypeUtils.isNumeric(flagComponents[1])) {
										parameters.setPerPage(Integer.parseInt(flagComponents[1]));
									} else {
										respond(sender, Prism.messenger.playerError("Per-page flag value must be a number. Use /prism ? for help."));
										return null;
									}
								} else if (flag.equals(Flag.SHARE)) {
									for (String sharePlayer : flagComponents[1].split(",")) {
										if (sharePlayer.equals(sender.getName())) {
											respond(sender, Prism.messenger.playerError("You can't share lookup results with yourself!"));
											return null;
										}
										Player shareWith = plugin.getServer().getPlayer(sharePlayer);
										if (shareWith != null) {
											parameters.addSharedPlayer((CommandSender) shareWith);
										} else {
											sender.sendMessage(Prism.messenger.playerError("Can't share with " + sharePlayer + ". Are they online?"));
										}
									}
								}
							}
						}
					}
					catch (IllegalArgumentException ex) {
						respond(sender, Prism.messenger.playerError("Unrecognized flag '" + val + "'. Use /prism ? [command] for help."));
						return null;
					}
				}
			}

			// Validate any required args are set
			if (foundArgs.isEmpty()) {
				respond(sender, Prism.messenger.playerError("You're missing valid parameters. Use /prism ? for assistance."));
				return null;
			}

			/**
			 * Enforce defaults, unless we're doing a delete or they've been disabled in the config
			 */
			if (!processType.equals(PrismProcessType.DELETE) && useDefaults) {
				// Radius default, apply only if player present
				if (!foundArgs.containsKey("r") && player != null) {
					if (parameters.allowsNoRadius()) {
						// We'll allow no radius.
					} else {
						parameters.setRadius(plugin.getConfig().getInt("prism.queries.default-radius"));
						parameters.addDefaultUsed("r:" + parameters.getRadius());
					}
				}
				// World default
				if (player != null && !foundArgs.containsKey("w") && !parameters.allowsNoRadius()) {
					parameters.setWorld(player.getWorld().getName());
				}
				// Time default
				if (!foundArgs.containsKey("t") && !foundArgs.containsKey("before") && !foundArgs.containsKey("since")) {
					Long date = translateTimeStringToDate(plugin, sender, plugin.getConfig().getString("prism.queries.default-time-since"));
					if (date == 0) {
						Prism.log("Error - date range configuration for prism.time-since is not valid");
						date = translateTimeStringToDate(plugin, sender, "3d");
					}
					parameters.setSinceTime(date);
					parameters.addDefaultUsed("t:" + plugin.getConfig().getString("prism.queries.default-time-since"));
				}
			}

			// Player location
			if (player != null && !plugin.getConfig().getBoolean("prism.queries.never-use-defaults") && parameters.getPlayerLocation() == null) {
				parameters.setMinMaxVectorsFromPlayerLocation(player.getLocation());
			}

		}
		return parameters;
	}

	/**
	 * @return
	 */
	public static Long translateTimeStringToDate(Prism plugin, CommandSender sender, String arg_value) {

		Long dateFrom = 0L;

		Pattern p = Pattern.compile("([0-9]+)(s|h|m|d|w)");
		Calendar cal = Calendar.getInstance();

		String[] matches = TypeUtils.preg_match_all(p, arg_value);
		if (matches.length > 0) {
			for (String match : matches) {

				Matcher m = p.matcher(match);
				if (m.matches()) {

					if (m.groupCount() == 2) {

						int tfValue = Integer.parseInt(m.group(1));
						String tfFormat = m.group(2);

						if (tfFormat.equals("w")) {
							cal.add(Calendar.WEEK_OF_YEAR, -1 * tfValue);
						}
						else if (tfFormat.equals("d")) {
							cal.add(Calendar.DAY_OF_MONTH, -1 * tfValue);
						}
						else if (tfFormat.equals("h")) {
							cal.add(Calendar.HOUR, -1 * tfValue);
						}
						else if (tfFormat.equals("m")) {
							cal.add(Calendar.MINUTE, -1 * tfValue);
						}
						else if (tfFormat.equals("s")) {
							cal.add(Calendar.SECOND, -1 * tfValue);
						} else {
							respond(sender, Prism.messenger.playerError("Invalid timeframe values for " + tfFormat + ". Use /prism ? for help."));
							return null;
						}
					}
				}
			}
			dateFrom = cal.getTime().getTime();
		}

		if (dateFrom == null) {
			respond(sender, Prism.messenger.playerError("Invalid timeframe values. Use /prism ? for help."));
		}

		return dateFrom;

	}
}
