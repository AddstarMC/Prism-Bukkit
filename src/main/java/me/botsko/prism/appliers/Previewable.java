package me.botsko.prism.appliers;

public interface Previewable {

    /**
     *
     */
    void setIsPreview(boolean is_preview);

    /**
     * @return
     */
    void preview();

    /**
     *
     */
    void cancel_preview();

    /**
     *
     */
    void apply_preview();

    /**
     * @return
     */
    void apply();
}