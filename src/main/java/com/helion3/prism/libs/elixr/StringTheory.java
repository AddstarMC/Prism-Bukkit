package com.helion3.prism.libs.elixr;

import static org.bukkit.ChatColor.WHITE;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import mkremins.fanciful.FancyMessage;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class StringTheory {
    
    public static interface TokenFilter {
        public void format( FancyMessage fancy, String tokenVal );
    }
    public static class StringBaseline {
        private final ChatColor baseColor;
        public StringBaseline( ChatColor baseColor ){
            this.baseColor = baseColor;
        }
        public void format( FancyMessage fancy ){
        }
        public ChatColor getColor(){
            return baseColor;
        }
    }
    
    private Map<String,String> tokens = new HashMap<String,String>();
    private Map<String,TokenFilter> filters = new HashMap<String,TokenFilter>();
    private Map<String,StringBaseline> baselines = new HashMap<String,StringBaseline>();
    
    
    /**
     * 
     */
    public StringTheory(){
        
        // Add a default message formatter
        baseline(":msg", new StringBaseline(WHITE){
            @Override
            public void format( FancyMessage fancy ){
                fancy.color(WHITE);
            }
        });
    }
    
    
    /**
     * Define a baseline message format, the color will be re-used
     * after each token insertion to maintain consistency.
     * @param key
     * @param value
     */
    public void baseline( String key, StringBaseline value ){
        baselines.put( key, value );
    }
    
    /**
     * Define a token filter/formatter. 
     * @param key
     * @param value
     */
    public void filter( String key, TokenFilter value ){
        filters.put( key, value );
    }
    
    /**
     * Assign a value to a token
     * @param key
     * @param value
     * @return
     */
    public StringTheory know( String key, String value ){
        tokens.put( key, value );
        return this;
    }
    public StringTheory know( String key, Float value ){
        tokens.put( key, ""+value );
        return this;
    }
    public StringTheory know( String key, int value ){
        tokens.put( key, ""+value );
        return this;
    }
    
    /**
     * Sends the final message
     * @param sender
     * @param content
     */
    public void send( CommandSender sender, String content ){
        FancyMessage msg = parse(content);
//        System.out.println("raw json: " + msg.toJSONString());
//        System.out.println("raw text: " + msg.toOldMessageFormat());
        if( sender instanceof Player ){
            msg.send( (Player)sender );
        } else {
            sender.sendMessage( text(content) );
        }
        tokens.clear();
    }

    /**
     * Return an original sendMessage-compatible string.
     * @param content
     * @return
     */
    protected String text( String content ){
        String text = parse(content).toOldMessageFormat();
        return text;
    }
    
    /**
     * Parse a string, using baselines, filters, and tokens
     * @param content
     * @return
     */
    public FancyMessage parse( String content ){

        FancyMessage fancy = new FancyMessage("");
        
        String[] exploded = content.split(" ");

        Pattern pattern = Pattern.compile("\\{.*?\\}");
        StringBaseline baseline = baselines.get(":msg");
        for( String seg : exploded ){
            
            // Pull a baseline formatter
            if( seg.startsWith(":") ){
                baseline = baselines.get( seg );
                if( baseline != null ){
                    baseline.format(fancy);
                    continue;
                }
            }
            
            String prefix = "";
            String suffix = "";
            String newWord = new String(seg).replaceAll("\\{.*\\}","|");
            String[] chars = newWord.split("\\|");
            if( chars.length > 1 ){
                prefix = chars[0];
            }
            if( chars.length == 2 ){
                suffix = chars[1];
            }

            Matcher matcher = pattern.matcher(seg);
            boolean matchFound = false;
            while(matcher.find()){
                
                fancy.then(prefix);
                
                matchFound = true;

                String completeToken = matcher.group(0).replace("{", "").replace("}", "");
                String[] token = completeToken.split("\\|");

                String val = "";

                if( !token[0].isEmpty() && tokens.containsKey(token[0]) ){
                    val = tokens.get(token[0]);
                }
                
                if( token.length == 2 && !token[1].isEmpty() && filters.containsKey(token[1]) ){
                    // if token was empty
                    if( token[0].isEmpty() ){
                        
                    } else {
                        TokenFilter filter = filters.get(token[1]);
                        filter.format(fancy,val);
                        fancy.then(suffix+" ");
                    }
                } else {
                    fancy.then(val).color( baseline.getColor() ).then(suffix+" ");
                }
            }
            if( !matchFound ){
                fancy.then(seg+" ").color( baseline.getColor() );
            }
        }
        
        return fancy;
        
    }
}