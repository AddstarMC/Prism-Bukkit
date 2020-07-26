package me.botsko.prism.database.sql;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.database.BlockReportQuery;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.utils.MaterialAliases;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.ChatColor;
import org.bukkit.block.data.BlockData;
import org.bukkit.command.CommandSender;
import org.bukkit.inventory.ItemStack;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

public class SqlBlockReportQueryBuilder extends SqlSelectQueryBuilder implements BlockReportQuery {

    public SqlBlockReportQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    @Override
    public String getQuery(QueryParameters parameters, boolean shouldGroup) {

        this.parameters = parameters;
        this.shouldGroup = shouldGroup;

        // Reset
        columns = new ArrayList<>();
        conditions = new ArrayList<>();

        String query = select();

        query += ";";

        Prism.debug(query);
        return query;

    }

    @Override
    protected String select() {
        parameters.addActionType("block-place");

        // block-place query
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT block_id, SUM(placed) AS placed, SUM(broken) AS broken ")
                .append("FROM ((").append("SELECT block_id, COUNT(id) AS placed, 0 AS broken ").append("FROM ")
                .append(prefix).append("data ").append(where()).append(" ").append("GROUP BY block_id) ");
        conditions.clear();
        parameters.getActionTypes().clear();
        parameters.addActionType("block-break");
        sql.append("UNION ( " + "SELECT block_id, 0 AS placed, count(id) AS broken ").append("FROM ")
                .append(prefix).append("data ").append(where()).append(" GROUP BY block_id)) ")
                .append("AS PR_A ").append("GROUP BY block_id ORDER BY (SUM(placed) + SUM(broken)) DESC");
        return sql.toString();

    }

    @Override
    public void report(CommandSender sender) {
        String playerName = null;
        for (String name : parameters.getPlayerNames().keySet()) {
            playerName = name;
        }
        sender.sendMessage(Prism.messenger.playerSubduedHeaderMsg(
                "Crafting block change report for " + ChatColor.DARK_AQUA + playerName + "..."));

        final int colTextLen = 20;
        final int colIntLen = 12;
        try (
                Connection conn = dataSource.getDataSource().getConnection();
                PreparedStatement s = conn.prepareStatement(getQuery(parameters, shouldGroup));
                ResultSet rs = s.executeQuery()

        ) {
            sender.sendMessage(Prism.messenger
                    .playerHeaderMsg("Total block changes for " + ChatColor.DARK_AQUA + playerName));
            sender.sendMessage(
                    Prism.messenger.playerMsg(ChatColor.GRAY + TypeUtils.padStringRight("Block", colTextLen)
                            + TypeUtils.padStringRight("Placed", colIntLen)
                            + TypeUtils.padStringRight("Broken", colIntLen)));
            while (rs.next()) {
                int blockId = rs.getInt(1);
                MaterialAliases.MaterialState state = Prism.getItems().idsToMaterial(blockId, 0, true);
                final String alias;
                if (state == null) {
                    alias = "UnknownMaterial_BlockId_" + blockId;
                } else {

                    BlockData block = state.asBlockData();
                    ItemStack item = state.asItem();

                    if (block != null) {
                        alias = Prism.getItems().getAlias(block.getMaterial(), block);
                    } else if (item != null) {
                        alias = Prism.getItems().getAlias(item);
                    } else {
                        alias = "InvalidState_" + state + "_BlockId_" + blockId;
                    }
                }

                final int placed = rs.getInt(2);
                final int broken = rs.getInt(3);

                final String colAlias = TypeUtils.padStringRight(alias, colTextLen);
                final String colPlaced = TypeUtils.padStringRight("" + placed, colIntLen);
                final String colBroken = TypeUtils.padStringRight("" + broken, colIntLen);

                sender.sendMessage(Prism.messenger.playerMsg(ChatColor.DARK_AQUA + colAlias
                        + ChatColor.GREEN + colPlaced + " " + ChatColor.RED + colBroken));

            }
        } catch (SQLException e) {
            dataSource.handleDataSourceException(e);
        }
    }
}