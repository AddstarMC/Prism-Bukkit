package me.botsko.prism.actions;

public class PlayerDeathAction extends GenericAction {

    /**
	 * 
	 */
    protected String cause;

    /**
	 * 
	 */
    protected String attacker;

    /**
     * 
     * @param cause
     */
    public void setCause(String cause) {
        this.cause = cause;
    }

    /**
     * 
     * @param attacker
     */
    public void setAttacker(String attacker) {
        this.attacker = attacker;
    }

    /**
	 * 
	 */
    @Override
    public void setData(String data) {
        this.data = data;
        if( cause == null && data != null ) {
            final String[] dataArr = data.split( ":" );
            cause = dataArr[0];
            if( dataArr.length > 1 ) {
                attacker = dataArr[1];
            }
        }
    }

    /**
	 * 
	 */
    @Override
    public void save() {
        if( data == null && cause != null ) {
            data = cause + ":" + attacker;
        }
    }

    /**
     * 
     * @return
     */
    @Override
    public String getNiceName() {
        String name = "";
        if( attacker != null && !attacker.isEmpty() ) {
            name += attacker;
        }
        if( cause != null && !cause.isEmpty() ) {
            name += "(" + cause + ")";
        }
        return name;
    }
}