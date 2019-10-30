package ro.razvan.uniud.oop.telephoneOperator.util;

import org.jetbrains.annotations.NotNull;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Collection;
import java.util.Map;

public final class JSONObjectBuilder {

    @NotNull
    private final JSONObject json;

    public JSONObjectBuilder() {
        json = new JSONObject();
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, @NotNull final Object value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, @NotNull final Map<?, ?> value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, @NotNull Collection<?> value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, final long value) {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, final boolean value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, final int value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, final double value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObjectBuilder put(@NotNull final String key, final float value) throws JSONException {
        json.put(key, value);
        return this;
    }

    public @NotNull
    JSONObject build() {
        return json;
    }
}
