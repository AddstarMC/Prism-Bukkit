package me.botsko.prism.database.mysql;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.database.ActionReportQuery;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

public class MySQLActionReportQueryBuilder extends MySQLSelectQueryBuilder implements ActionReportQuery {

	/**
	 *
	 **/
	public MySQLActionReportQueryBuilder() {
		super();
	}

	/**
	 * @param parameters
	 * @param shouldGroup
	 * @return
	 */
	@Override
	public String getQuery(QueryParameters parameters, boolean shouldGroup) {

		this.parameters = parameters;
		this.shouldGroup = shouldGroup;

		// Reset
		columns = new ArrayList<>();
		conditions = new ArrayList<>();

		String query = select();

		query += ";";

		if (Prism.config.getBoolean("prism.debug")) {
			Prism.debug(query);
		}

		return query;

	}

	/**
	 *
	 */
	@Override
	public String select() {
		String prefix = Prism.config.getString("prism.mysql.prefix");

		final String sql = "SELECT COUNT(*), a.action " + "FROM " + prefix + "data " + "INNER JOIN " + prefix
				+ "actions a ON a.action_id = " + prefix + "data.action_id " + where() + " " + "GROUP BY a.action_id "
				+ "ORDER BY COUNT(*) DESC";

		return sql;

	}

	@Override
	public void report(CommandSender sender) {
		String tempName = "";
		for (final String player : parameters.getPlayerNames().keySet()) {
			tempName = player;
			break;
		}
		final String playerName = tempName;
		final int colTextLen = 16;
		final int colIntLen = 12;
		sender.sendMessage(Prism.messenger.playerSubduedHeaderMsg(
				"Crafting action type report for " + ChatColor.DARK_AQUA + playerName + "..."));
		try (
				Connection conn = Prism.getPrismDataSource().getConnection();
				PreparedStatement s = conn.prepareStatement(getQuery(parameters, shouldGroup));
				ResultSet rs = s.executeQuery();
		) {
			sender.sendMessage(
					Prism.messenger.playerMsg(ChatColor.GRAY + TypeUtils.padStringRight("Action", colTextLen)
							+ TypeUtils.padStringRight("Count", colIntLen)));
			while (rs.next()) {
				final String action = rs.getString(2);
				final int count = rs.getInt(1);

				final String colAlias = TypeUtils.padStringRight(action, colTextLen);
				final String colPlaced = TypeUtils.padStringRight("" + count, colIntLen);
				sender.sendMessage(Prism.messenger
						.playerMsg(ChatColor.DARK_AQUA + colAlias + ChatColor.GREEN + colPlaced));

			}
		} catch (final SQLException e) {
			Prism.getPrismDataSource().handleDataSourceException(e);
		}
	}
}
