package me.botsko.prism.actions;

import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;

public class PrismProcessAction extends GenericAction {

    /**
     * The extra data.
     */
    private PrismProcessActionData actionData;

    /**
     * Process.
     * @param processType PrismProcessType
     * @param parameters String
     */
    public void setProcessData(PrismProcessType processType, String parameters) {

        actionData = new PrismProcessActionData();

        if (processType != null) {
            actionData.params = parameters;
            actionData.processType = processType.name().toLowerCase();
        }
    }

    @Override
    public boolean hasExtraData() {
        return actionData != null;
    }

    @Override
    public String serialize() {
        return gson().toJson(actionData);
    }

    @Override
    public void deserialize(String data) {
        if (data != null && !data.isEmpty()) {
            actionData = gson().fromJson(data, PrismProcessActionData.class);
        }
    }

    /**
     * Get Type.
     * @return String
     */
    public String getProcessChildActionType() {
        return Prism.getActionRegistry().getAction("prism-" + actionData.processType).getName();
    }

    /**
     * Get nice name.
     */
    @Override
    public String getNiceName() {
        return actionData.processType + " (" + actionData.params + ")";
    }

    public static class PrismProcessActionData {
        public String params = "";
        public String processType;
    }
}