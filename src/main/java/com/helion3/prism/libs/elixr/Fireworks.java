package com.helion3.prism.libs.elixr;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.bukkit.Color;
import org.bukkit.FireworkEffect;
import org.bukkit.FireworkEffect.Type;
import org.bukkit.Location;
import org.bukkit.entity.Firework;
import org.bukkit.inventory.meta.FireworkMeta;

public class Fireworks {
	
	/**
	 * 
	 */
	private static final Random rng = new Random();
	
	/**
	 * 
	 * @param loc
	 */
	public static void launchRandomFireworks( Location loc ){
	    int count = rng.nextInt( 5 ) + 1;
	    for( int i = 0; i < count; i++ ){
	        launchRandomFirework(loc);
	    }
	}
	
	/**
	 * 
	 * @param loc
	 */
	public static void launchRandomFirework( Location loc ){
		
	    List<Type> types = new ArrayList<Type>();
        types.add(Type.BALL);
        types.add(Type.BALL_LARGE);
        types.add(Type.BURST);
        types.add(Type.CREEPER);
        types.add(Type.STAR);
		
		final Firework firework = loc.getWorld().spawn(loc, Firework.class);
		FireworkMeta fireworkMeta = null;
		fireworkMeta = determineEffect( types, firework );
		if( fireworkMeta == null ) return;
		firework.setFireworkMeta(fireworkMeta);
		
	}
	
	/**
	 * Creeper
	 * @param firework
	 */
	protected static FireworkMeta determineEffect( List<Type> allowedTypes, Firework firework ){
		
        final FireworkMeta fireworkMeta = firework.getFireworkMeta();
        
        // Rand power
        fireworkMeta.setPower( rng.nextInt(2)+1 ); // 1-3
        
        // Effects
        FireworkEffect.Builder effect = FireworkEffect.builder();
        
        Color c1 = Color.fromRGB(rng.nextInt(255), rng.nextInt(255), rng.nextInt(255));
        Color c2 = Color.fromRGB(rng.nextInt(255), rng.nextInt(255), rng.nextInt(255));
        Color fade = Color.fromRGB(rng.nextInt(255), rng.nextInt(255), rng.nextInt(255));
        
        // Rand styles
        effect.withColor(c1, c2);
        effect.withFade(fade);
        
        // Rand style based on the user perms
        effect.with( allowedTypes.get( rng.nextInt( allowedTypes.size() ) ) );
        
        if( rng.nextBoolean() ) effect.withFlicker();
        
        if( rng.nextBoolean() ) effect.withTrail();
        
        fireworkMeta.addEffect(effect.build());
        return fireworkMeta;
        
	}
}