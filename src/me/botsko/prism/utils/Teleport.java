package me.botsko.prism.utils;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;

public class Teleport {
	
	
	/**
	 * @todo this needs an overhaul. we need to tp to a nearby safe area,
	 * etc.
	 * @param loc
	 * @return
	 */
	public static boolean isSafe(World world, Location loc){
	    double y = loc.getY();
	    // Check for suffocation
	    loc.setY(y+1);
	    Block block1 = world.getBlockAt(loc);
	    loc.setY(y+2);
	    Block block2 = world.getBlockAt(loc);
	    if(!(block1.getTypeId()==0||block2.getTypeId()==0)){
	        return false; //not safe, suffocated
	    }
	    //Check for lava/void
	    for(double i=128;i>-1;i--){
	        loc.setY(i);
	        Block block = world.getBlockAt(loc);
	        if(block.getTypeId()!=0){
	            if(block.getType()==Material.LAVA){
	                return false;//not safe, lava above or below you
	            } else {
	                if(!(block.getType()==Material.TORCH||block.getType()==Material.REDSTONE_TORCH_ON||block.getType()==Material.REDSTONE_TORCH_OFF||block.getType()==Material.PAINTING)){
	                    if(i<y){
	                        loc.setY(-1);//set y to negitive 1 to end loop, we hit solid ground.
	                    }
	                    //Check for painful fall
	                    if((y-i)>10){
	                        return false;//would fall down at least 11 blocks = painful landing...
		                }
		            } else {
			            if(i==0){
			                if(block.getTypeId()==0){
			                    return false; // not safe, void
			                }
			            }
			        }
	            }
		    }
		}
	    return true;
	}
}
