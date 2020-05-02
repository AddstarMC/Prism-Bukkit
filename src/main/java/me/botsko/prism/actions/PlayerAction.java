package me.botsko.prism.actions;

public class PlayerAction extends GenericAction {

    private String extraInfo;

    /**
     * @return
     */
    @Override
    public String getNiceName() {
        if (extraInfo != null && !extraInfo.isEmpty()) {
            switch (getActionType().getName()) {
                case "player-join":
                    return "from " + extraInfo;

                case "xp-pickup":
                    return extraInfo + " xp";

                case "bucket-fill":
                    return "a " + extraInfo + " bucket";

                default:
                    return extraInfo;
            }
        }

        return "";
    }

    @Override
    public boolean hasExtraData() {
        return extraInfo != null;
    }

    @Override
    public String serialize() {
        return extraInfo;
    }

    @Override
    public void deserialize(String data) {
        extraInfo = data;
    }
}