package me.botsko.prism.actions;

public class PlayerAction extends GenericAction {

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        if( this.data != null && !this.data.isEmpty() ) {
            if( this.type.getName().equals( "player-join" ) ) {
                return "from " + this.data;
            } else if( this.type.getName().equals( "xp-pickup" ) ) {
                return this.data + " xp";
            } else if( this.type.getName().equals( "bucket-fill" ) ) {
                return "a " + this.data + " bucket";
            } else {
                return this.data;
            }
        }
        return "";
    }
}