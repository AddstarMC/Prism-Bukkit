package me.botsko.prism.appliers;

public interface Previewable {

    /**
     * Set if preview.
     */
    void setIsPreview(boolean isPreview);

    /**
     * Preview.
     */
    void preview();

    /**
     * Cancel the preview.
     */
    void cancel_preview();

    /**
     * Apply the preview.
     */
    void apply_preview();

    /**
     * Apply the preview to the world. Make the changes.
     */
    void apply();
}