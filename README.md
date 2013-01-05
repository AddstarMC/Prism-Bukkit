#Prism

Prism is a new generation of grief management (rollbacks, restores, etc) plugin for your Bukkit server. It's designed to be extremely fast and incredibly powerful - designed by a group with a lot of experience in both programming and owning/managing a very popular/high-traffic server.

## Features

- **Extremely fast. Rolls back thousands of broken blocks within milliseconds. No lag.**
- **Several easy, yet powerful lookup/inspector commands.**
- **Rollback, restoration commands like you know and love.**
- **Preview mode shows rollback AND restore actions to you only - then you can apply/cancel before actually changing the world.**
- **Tracks essentially everything you need to know. (Configurable, see below)**
- **Amazingly easy-to-use commands**
- **Safe defaults so you don't accidentally roll back too much with a typo**
- **Extremely clean, personal, and beautiful chat messages, that NEVER spam you.**
- **Configurable for your needs. Change the defaults, ignore players/worlds, ignore creative, disable tracking events you don't care about!**
- Tracks essentially ALL block changes.
- Includes related changes like blocks falling, signs/torches popping off
- Tracks sign text and even restores it upon rollbacks.
- Associates those related activities to the original block breaking player.
- Mushrooms and tree growth is associated with the planting player.
- Rolling back fire damage also puts out any remaining fires. (Configurable)
- Or just use the extinguish command.
- Rolling back tnt/creeper explosions also removes the laggy item drops within the radius. (Configurable)
- Drain water/lava with the drain command.
- Track and rollback animal/mob kills. Someone killed your cows? No problem.*
- Track shearing of sheep.
- Track mobs changing blocks like enderman, sheep eating, etc.
- Tracks chest/furnace/dispenser/etc access. 
- Watches for someone placing/taking items from the containers.
- You can even rollback items stolen from the container.*
- Records player deaths and what the cause was, who the attacker was.
- Records items player dropped, and items they pickup. Know who stole your death spot loot!
- Find the right info with paginated near/lookup commands.
- Get specific with a block history inspector bound to your hand.
- Database is designed specifically for the data we store. No disk space bloating (not going to point fingers...)



## How to Install
	
Place the Prism.jar file in your bukkit `/plugins` directory. Start your server and an initial configuration file will be created.

At the very least, you must set the MySQL connection information for your database before using Prism.
	

## How to Use


#### Searching (Lookup, Inspector, Near)

#### Understanding Parameters


#### Preview

#### Rollback

@todo make warning about items from containers (prism.appliers.allow_rollback_items_removed_from_container)

#### Restore




## Commands

- `prism i`
- `prism l (params)`
- `prism near`
- `prism page [#]`
- `prism preview rollback (params)`
- `prism preview restore (params)`
- `prism preview apply`
- `prism preview cancel`
- `prism rollback (params)`
- `prism restore (params)`
- `prism ex [radius]`
- `prism drain [radius]`
- `prism tp [id]]`
- `prism delete`
- `prism credits`

#### Parameters:

Parameters are to be used for any command that supports (params). You may define any, all, or none. They
help you find, rollback, or restore essentially any piece of information you need. Some use defaults and
some don't.

You can even define multiple arguments for most by separating them with a comma. Like "a:block-break,block-place"

- `a:[action]` - Like "block-break" (See below for full list). No default.
- `r:[radius]` - i.e. 20, or 100. Defaults to default-radius defined in config. Use r:global to force an all-world search, for lookups only.
- `b:[blockname/id]` - Like "grass" or "2" or "2:0". No default.
- `e:[entity]` - Like "pig". No default.
- `t:[timesince]` - Events after x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). No default.
- `p:[player]` - Like "viveleroi". No default.
- `w:[world]` - Defaults to your current world.


#### Actions

(todo)

## Permissions

- `prism.help`
- `prism.lookup` - Grants config lookup, inspector, near permission. Recommended for: Staff
- `prism.extinguish` - For removed fires in a radius.
- `prism.drain` - Removing water/lava in a radius.
- `prism.preview` - Grants permission to preview and then rollback/restore changes. Recommended for: Staff
- `prism.rollback` - Grants permission to rollback changes. Recommended for: Experienced Staff
- `prism.restore` - Grants permission to restore (reapply) changes. Recommended for: Experienced Staff       
- `prism.tp` - Teleport to a record ID
- `prism.reload` - Grants config reload permission. Recommended for: OPs
- `prism.delete` - Purge records from database via commands. Recommended for: OPs
- `prism.*` - Grants all permissions. Recommended for: OPs

## Known Issues

- Hanging items can't be rolled back because of a bukkit bug with their facing directions. https://bukkit.atlassian.net/browse/BUKKIT-3371

          
## Get Help

IRC: irc.esper.net #dhmc_us

Wiki: http://dhmc.us/wiki/view/prism/          
           
## Credits

This plugin was custom designed for the amazing *dhmc.us* Minecraft server.


## Authors

- viveleroi (Creator, Lead Developer)
- jasonbbb711 (Contributor)