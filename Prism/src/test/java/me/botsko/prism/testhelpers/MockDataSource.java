package me.botsko.prism.testhelpers;

import me.botsko.prism.api.actions.Handler;
import me.botsko.prism.database.InsertQuery;
import me.botsko.prism.database.PlayerIdentificationQuery;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.sql.SqlInsertBuilder;
import me.botsko.prism.players.PrismPlayer;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class MockDataSource {

    public static ArgumentCaptor<String> playerName = ArgumentCaptor.forClass(String.class);
    public static ArgumentCaptor<UUID> playerUuid = ArgumentCaptor.forClass(UUID.class);

    public static PrismDataSource getDataSource(){
        PrismDataSource dataSource = Mockito.mock(PrismDataSource.class);
        when(dataSource.createDataSource()).thenReturn(dataSource);
        when(dataSource.reportDataSource(any(StringBuilder.class))).thenReturn(true);
        InsertQuery query = Mockito.spy(new SqlInsertBuilder(dataSource));
        when(query.insertActionIntoDatabase(any(Handler.class))).thenReturn(Long.valueOf(1));
        PrismPlayer player = new PrismPlayer(1, UUID.randomUUID(),"TestPlayer");
        PlayerIdentificationQuery playerQuery = Mockito.mock(PlayerIdentificationQuery.class);
        doNothing().when(playerQuery).addPlayer(playerName.capture(),playerUuid.capture());
        when(playerQuery.lookupByUuid(any(UUID.class)))
                .then((Answer<PrismPlayer>) invocation -> {
                    if(invocation.getArgument(1) == playerUuid.getValue()){
                        return new PrismPlayer(2, playerUuid.getValue(), playerName.getValue());
                    }
                    return null;
                });
        when(dataSource.getPlayerIdHelper()).thenReturn(playerQuery);
        when(dataSource.getPlayerIdHelper().lookupByName(anyString())).thenReturn(player);
        when(dataSource.getName()).thenReturn("test");
        when(dataSource.getDataInsertionQuery()).thenReturn(query);
        when(dataSource.getPlayerIdHelper().addFakePlayer(anyString())).thenReturn(player);

        return dataSource;
    }



}

