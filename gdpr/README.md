Scala Compiler plugin for GDPR compliance
---

## Compiling and running the plugin

In the `plugin` folder:
```
scalac -d classes plugin.scala
cd classes
jar cf ../gdpr.jar .
```
Or run `build.sh`.

In the source folder:
```
scalac -Xplugin:./plugin/gdpr.jar -Xplugin-list # shows that the plugin is loaded correctly
scalac -Xplugin:./plugin/gdpr.jar data.scala
```