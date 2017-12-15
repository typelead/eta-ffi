package eta.ffi;

import java.util.Set;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLClassLoader;
import java.net.MalformedURLException;

import com.google.common.base.Predicate;

import org.reflections.Reflections;
import org.reflections.util.ConfigurationBuilder;
import org.reflections.scanners.SubTypesScanner;

public class Utils {
    public static Set<Class<? extends Object>>
        getClasses(String[] classpath, Predicate<String> predicate) {
        try {
            URL[] urls = new URL[classpath.length];
            for (int i = 0; i < classpath.length; i++) {
                urls[i] = new File(classpath[i]).toURI().toURL();
            }
            return new Reflections(new ConfigurationBuilder()
                                   .addClassLoader(new URLClassLoader(urls))
                                   .addUrls(urls)
                                   .setScanners(new SubTypesScanner(false))
                                   .filterInputsBy(predicate)
                                   .useParallelExecutor())
                .getSubTypesOf(Object.class);
        } catch (MalformedURLException me) {
            return null;
        }
    }
}
