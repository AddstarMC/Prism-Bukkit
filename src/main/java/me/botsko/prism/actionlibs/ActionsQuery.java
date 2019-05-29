package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.data.BlockData;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.database.mysql.DeleteQueryBuilder;
import me.botsko.prism.database.mysql.SelectQueryBuilder;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.utils.MaterialAliases.MaterialState;

public class ActionsQuery {

	/**
	 * 
	 */
	private final Prism plugin;

	/**
	 * 
	 */
	private final SelectQueryBuilder qb;

	/**
	 * 
	 */
	private boolean shouldGroup = false;

	/**
	 * 
	 * @param plugin
	 * @return
	 */
	public ActionsQuery(Prism plugin) {
		this.plugin = plugin;
		this.qb = new SelectQueryBuilder(plugin);
	}

	/**
	 * 
	 * @return
	 */
	public QueryResult lookup(QueryParameters parameters) {
		return lookup(parameters, null);
	}

	/**
	 * 
	 * @return
	 */
	public QueryResult lookup(QueryParameters parameters, CommandSender sender) {

		Player player = null;
		if (sender instanceof Player) {
			player = (Player) sender;
		}

		// If lookup, determine if we need to group
		shouldGroup = false;
		if (parameters.getProcessType().equals(PrismProcessType.LOOKUP)) {
			shouldGroup = true;
			// What to default to
			if (!plugin.getConfig().getBoolean("prism.queries.lookup-auto-group")) {
				shouldGroup = false;
			}
			// Any overriding flags passed?
			if (parameters.hasFlag(Flag.NO_GROUP) || parameters.hasFlag(Flag.EXTENDED)) {
				shouldGroup = false;
			}
		}

		// Pull results
		final List<Handler> actions = new ArrayList<Handler>();

		// Build conditions based off final args
		final String query = qb.getQuery(parameters, shouldGroup);

		if (query != null) {
			Connection conn = null;
			PreparedStatement s = null;
			ResultSet rs = null;
			try {

				plugin.eventTimer.recordTimedEvent("query started");

				conn = Prism.dbc();

				// Handle dead connections
				if (conn == null || conn.isClosed()) {
					if (RecordingManager.failedDbConnectionCount == 0) {
						Prism.log(
								"Prism database error. Connection should be there but it's not. Leaving actions to log in queue.");
					}
					RecordingManager.failedDbConnectionCount++;
					sender.sendMessage(
							Prism.messenger.playerError("Database connection was closed, please wait and try again."));
					return new QueryResult(actions, parameters);
				}
				else {
					RecordingManager.failedDbConnectionCount = 0;
				}

				s = conn.prepareStatement(query);
				rs = s.executeQuery();

				plugin.eventTimer.recordTimedEvent("query returned, building results");
				
				Map<Integer, String> worldsInverse = new HashMap<>();
				
				for (final Entry<String, Integer> entry : Prism.prismWorlds.entrySet()) {
					worldsInverse.put(entry.getValue(), entry.getKey());
				}

				while (rs.next()) {

					if (rs.getString(3) == null)
						continue;

					// Convert action ID to name
					// Performance-wise this is a lot faster than table joins
					// and the cache data should always be available
					int actionId = rs.getInt(3);

					String actionName = "";
					for (final Entry<String, Integer> entry : Prism.prismActions.entrySet()) {
						if (entry.getValue() == actionId) {
							actionName = entry.getKey();
						}
					}
					if (actionName.isEmpty()) {
						Prism.warn("Record contains action ID that doesn't exist in cache: " + actionId + ", cacheSize=" + Prism.prismActions.size());
						continue;
					}

					// Get the action handler
					final ActionType actionType = Prism.getActionRegistry().getAction(actionName);

					if (actionType == null)
						continue;

					// Prism.debug("Important: Action type '" + rs.getString(3)
					// +
					// "' has no official handling class, will be shown as generic."
					// );

					long rowId = 0;

					try {

						final Handler baseHandler = Prism.getHandlerRegistry().create(actionType.getHandler());

						// Convert world ID to name
						// Performance-wise this is typically a lot faster than
						// table joins
						String worldName = worldsInverse.getOrDefault(rs.getInt(5), "");

						rowId = rs.getLong(1);

						// Set all shared values
						baseHandler.setActionType(actionType);
						baseHandler.setId(rowId);
						baseHandler.setUnixEpoch(rs.getLong(2));
						baseHandler.setWorld(Bukkit.getWorld(worldName));
						baseHandler.setX(rs.getInt(6));
						baseHandler.setY(rs.getInt(7));
						baseHandler.setZ(rs.getInt(8));

						int blockId = rs.getInt(9);
						int blockSubId = rs.getInt(10);

						int oldBlockId = rs.getInt(11);
						int oldBlockSubId = rs.getInt(12);

						String itemMetadata = rs.getString(13);

						Boolean validBlockId = false;
						Boolean validOldBlockId = false;

						MaterialState current = Prism.getItems().idsToMaterial(blockId, blockSubId, false);

						if (current != null) {
							ItemStack item = current.asItem();
							BlockData block = current.asBlockData();

							if (block != null) {
								validBlockId = true;
								baseHandler.setMaterial(block.getMaterial());
								baseHandler.setBlockData(block);
								baseHandler.setDurability((short) 0);

							} else if (item != null) {
								validBlockId = true;
								baseHandler.setMaterial(item.getType());

								BlockData newData;

								try {
									newData = Bukkit.createBlockData(item.getType());
								} catch (IllegalArgumentException e) {
									// This exception occurs, for example, with "ItemStack{DIAMOND_LEGGINGS x 1}"
									Prism.debug("IllegalArgumentException for record #" + rowId + " calling createBlockData for " + item.toString());
									newData = null;
								}

								baseHandler.setBlockData(newData);
								baseHandler.setDurability((short) ItemUtils.getItemDamage(item));
							}
						}

						MaterialState old = Prism.getItems().idsToMaterial(oldBlockId, oldBlockSubId, false);

						if (old != null) {
							ItemStack oldItem = old.asItem();
							BlockData oldBlock = old.asBlockData();

							if (oldBlock != null) {
								validOldBlockId = true;
								baseHandler.setOldMaterial(oldBlock.getMaterial());
								baseHandler.setOldBlockData(oldBlock);
								baseHandler.setOldDurability((short) 0);
							}
							else {
								validOldBlockId = true;
								baseHandler.setOldMaterial(oldItem.getType());
								baseHandler.setOldBlockData(Bukkit.createBlockData(oldItem.getType()));
								baseHandler.setOldDurability((short) ItemUtils.getItemDamage(oldItem));
							}
						}

						if (!validBlockId && !validOldBlockId) {
							// Entry could not be converted to a block or an item

							Boolean logWarning;
							if (blockId == 0 && oldBlockId == 0 && itemMetadata != null && itemMetadata.contains("entity_name")) {
								// The current item is likely a spawn or death event for an entity, for example, a cow or horse
								logWarning = false;
							} else {
								logWarning = true;
							}

							if (logWarning) {
								String itemMetadataDesc;

								if (itemMetadata == null) {
									itemMetadataDesc = "";
								} else {
									itemMetadataDesc = ", metadata=" + itemMetadata;
								}

								if (blockId > 0) {
									Prism.warn("Unable to convert record #" + rowId + " to material: " +
											"block_id=" + blockId + ", block_subid=" + blockSubId + itemMetadataDesc);
								} else if (oldBlockId > 0) {
									Prism.warn("Unable to convert record #" + rowId + " to material: " +
											"old_block_id=" + oldBlockId + ", old_block_subid=" + oldBlockSubId + itemMetadataDesc);
								} else {
									Prism.warn("Unable to convert record #" + rowId + " to material: " +
											"block_id=0, old_block_id=0" + itemMetadataDesc);
								}
							}
						}

						// data
						baseHandler.deserialize(itemMetadata);
						
						// player
						baseHandler.setSourceName(rs.getString(4));
						
						// player_uuid
						try {
							// Calls UUID.fromString, must handle potential exceptions
							OfflinePlayer offline = Bukkit.getOfflinePlayer(
									PlayerIdentification.uuidFromDbString(rs.getString(14)));
							
							// Fake player
							if(offline.hasPlayedBefore()) {
								baseHandler.setUUID(offline.getUniqueId());
							}
						}
						catch(IllegalArgumentException | NullPointerException e) {
							// Not a valid uuid
						}

						// Set aggregate counts if a lookup
						int aggregated = 0;
						if (shouldGroup) {
							aggregated = rs.getInt(15);
						}
						baseHandler.setAggregateCount(aggregated);

						actions.add(baseHandler);

					}
					catch (final SQLException e) {
						Prism.warn("Ignoring data from record #" + rowId + " because it caused an error:");
						e.printStackTrace();
					}
				}
			}
			catch (final SQLException e) {
				plugin.handleDatabaseException(e);
			}
			finally {
				if (rs != null)
					try {
						rs.close();
					}
					catch (final SQLException ignored) {
					}
				if (s != null)
					try {
						s.close();
					}
					catch (final SQLException ignored) {
					}
				if (conn != null)
					try {
						conn.close();
					}
					catch (final SQLException ignored) {
					}
			}
		}

		// Build result object
		final QueryResult res = new QueryResult(actions, parameters);
		res.setPerPage(parameters.getPerPage());

		// Cache it if we're doing a lookup. Otherwise we don't
		// need a cache.
		if (parameters.getProcessType().equals(PrismProcessType.LOOKUP)) {
			String keyName = "console";
			if (player != null) {
				keyName = player.getName();
			}
			if (plugin.cachedQueries.containsKey(keyName)) {
				plugin.cachedQueries.remove(keyName);
			}
			plugin.cachedQueries.put(keyName, res);
			// We also need to share these results with the -share-with players.
			for (final CommandSender sharedPlayer : parameters.getSharedPlayers()) {
				plugin.cachedQueries.put(sharedPlayer.getName(), res);
			}
		}

		plugin.eventTimer.recordTimedEvent("results object completed");

		// Return it
		return res;

	}

	/**
	 * 
	 * @param playername
	 */
	public long getUsersLastPrismProcessId(String playername) {
		String prefix = plugin.getConfig().getString("prism.mysql.prefix");
		long id = 0;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			final int action_id = Prism.prismActions.get("prism-process");

			conn = Prism.dbc();

			if (conn != null && !conn.isClosed()) {
				s = conn.prepareStatement(
						"SELECT id FROM " + prefix + "data JOIN " + prefix + "players p ON p.player_id = " + prefix
								+ "data.player_id WHERE action_id = ? AND p.player = ? ORDER BY id DESC LIMIT 1");
				s.setInt(1, action_id);
				s.setString(2, playername);
				rs = s.executeQuery();

				if (rs.first()) {
					id = rs.getLong("id");
				}
			}
			else {
				Prism.log("Prism database error. getUsersLastPrismProcessId cannot continue.");
			}
		}
		catch (final SQLException e) {
			plugin.handleDatabaseException(e);
		}
		finally {
			if (rs != null)
				try {
					rs.close();
				}
				catch (final SQLException ignored) {
				}
			if (s != null)
				try {
					s.close();
				}
				catch (final SQLException ignored) {
				}
			if (conn != null)
				try {
					conn.close();
				}
				catch (final SQLException ignored) {
				}
		}
		return id;
	}

	/**
	 * 
	 * @param id
	 */
	public PrismProcessAction getPrismProcessRecord(long id) {
		String prefix = plugin.getConfig().getString("prism.mysql.prefix");
		PrismProcessAction process = null;
		Connection conn = null;
		PreparedStatement s = null;
		ResultSet rs = null;
		try {

			String sql = "SELECT id, action, epoch, world, player, player_uuid, x, y, z, data FROM " + prefix
					+ "data d";
			// Joins
			sql += " INNER JOIN " + prefix + "players p ON p.player_id = d.player_id ";
			sql += " INNER JOIN " + prefix + "actions a ON a.action_id = d.action_id ";
			sql += " INNER JOIN " + prefix + "worlds w ON w.world_id = d.world_id ";
			sql += " LEFT JOIN " + prefix + "data_extra ex ON ex.data_id = d.id ";
			sql += " WHERE d.id = ?";

			conn = Prism.dbc();

			if (conn != null && !conn.isClosed()) {
				s = conn.prepareStatement(sql);
				s.setLong(1, id);
				rs = s.executeQuery();

				if (rs.first()) {
					process = new PrismProcessAction();
					// Set all shared values
					process.setId(rs.getLong("id"));
					process.setActionType(rs.getString("action"));
					process.setUnixEpoch(rs.getLong("epoch"));
					process.setWorld(Bukkit.getWorld(rs.getString("world")));
					process.setSourceName(rs.getString("player"));
					process.setUUID(UUID.fromString(rs.getString("player_uuid")));
					process.setX(rs.getInt("x"));
					process.setY(rs.getInt("y"));
					process.setZ(rs.getInt("z"));
					process.deserialize(rs.getString("data"));
				}
			}
			else {
				Prism.log("Prism database error. getPrismProcessRecord cannot continue.");
			}
		}
		catch (final SQLException e) {
			plugin.handleDatabaseException(e);
		}
		finally {
			if (rs != null)
				try {
					rs.close();
				}
				catch (final SQLException ignored) {
				}
			if (s != null)
				try {
					s.close();
				}
				catch (final SQLException ignored) {
				}
			if (conn != null)
				try {
					conn.close();
				}
				catch (final SQLException ignored) {
				}
		}
		return process;
	}

	/**
	 * 
	 * @return
	 */
	public int delete(QueryParameters parameters) {
		int total_rows_affected = 0, cycle_rows_affected;
		Connection conn = null;
		Statement s = null;
		try {
			final DeleteQueryBuilder dqb = new DeleteQueryBuilder(plugin);
			// Build conditions based off final args
			// shouldGroup does nothing for DeleteQueryBuilder, just use 'false' for clarity
			final String query = dqb.getQuery(parameters, false);
			conn = Prism.dbc();
			if (conn != null && !conn.isClosed()) {
				s = conn.createStatement();
				cycle_rows_affected = s.executeUpdate(query);
				total_rows_affected += cycle_rows_affected;
			}
			else {
				Prism.log("Prism database error. Purge cannot continue.");
			}
		}
		catch (final SQLException e) {
			plugin.handleDatabaseException(e);
		}
		finally {
			if (s != null)
				try {
					s.close();
				}
				catch (final SQLException ignored) {
				}
			if (conn != null)
				try {
					conn.close();
				}
				catch (final SQLException ignored) {
				}
		}
		return total_rows_affected;
	}
}