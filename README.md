# Installationsanleitung
Es gibt zwei Arten der Installation bzw. des Betriebes: Als lokale Standalone-Anwendung oder, speziell während der aktiven Entwicklung sinnvoll, integriert in einer Continuous-Deployment-Pipline mit Hilfe von [dokku](http://dokku.viewdocs.io/dokku/).

## Voraussetzung
Die Anwendung selbst ist in [Clojure](http://www.clojure.org) implementiert, einem funktionalen Lisp-Dialekt auf der [Java](http://www.java.com)-VM. Voraussetzung für das Kompilieren der Anwendung ist die Installation des Buildtools [Leiningen](http://leiningen.org/).

Die eingesetzte Datenbank ist die [Infobright Community Edition](https://infobright.com/blog/the-final-frontiers-of-ice/), einer spaltenorientierten relationalen Datenbank auf Basis von MySQL. Alternativ sollte auch MySQL oder MariaDB als Datenbank möglich sein, die Verarbeitungsgeschwindigkeit wäre jedoch niedriger und der notwendige Speicherbedarf höher. 

## Kompilieren der Anwendung
Die Anwendung selbst kann mit ```lein uberjar``` erstellt werden. Im Ordner `target` findet sich dann eine Datei `scada-ui.jar`. Diese kann gestartet werden mit `java -jar scada-ui.jar`.

## Verwendung von dokku - Continuous Deployment
Die Anwendung selbst sowie die Datenbank laufen in zwei getrennten [docker](https://www.docker.com/)-Containern im gleichen Server. Die Container selbst werden nach einem Neustart des Servers neu gestartet. Manuelle Veränderungen können mit den Kommandozeilenbefehlen von [dokku](http://dokku.viewdocs.io/dokku/) selbst vorgenommen werden, Details dazu in deren Dokumentation.

- Installiere [docker](https://www.docker.com/) und [dokku](http://dokku.viewdocs.io/dokku/)
  - Installiere das [Infobright-Plugin](https://github.com/smee/dokku-infobright) für dokku: `dokku plugin:install https://github.com/dokku/dokku-dokku.git ib`
  - Installiere Infobrightcontainer von https://github.com/meatcar/docker-infobright mit `docker pull meatcar/docker-infobright`
- Konfiguriere dokku:
  - initiale Konfiguration laut der [dokku-Anleitung](http://dokku.viewdocs.io/dokku/getting-started/installation/#installing-the-latest-stable-version)
  - im Verzeichnis mit dem Quellcode der Anwendung:	
    - `git remote add production dokku@SERVERNAME:gt1-ui`
	- `ssh dokku@SERVERNAME apps:create gt1-ui` erstelle eine neue Dokku-Applikation
	- `ssh dokku@SERVERNAME ib:create gt1-db` lege Datenbankcontainer an
	- `ssh dokku@SERVERNAME ib:link gt1-db gt1-ui` erlaube Zugriff durch die Anwendung auf die Datenbank, übergibt URL, Passwort etc.
	- `ssh dokku@SERVERNAME config:set gt1-ui IMPORT_FOLDER=/import` Pfad für Übergabe von CSV-Dateien an die Datenbank
	- `ssh dokku@SERVERNAME config:set gt1-ui INPUT_FOLDER=/data` Pfad für das Einlesen von Messdaten im Anwendungscontainer
	- `ssh dokku@SERVERNAME config:set gt1-ui JVM_OPTS=-Xmx3G` Speichereinstellungen für die JVM
	- `ssh dokku@SERVERNAME docker-options:add gt1-ui run,deploy -v /etc/localtime:/etc/localtime:ro` verwende die gleiche Zeitzone in den Container wie der Server selbst
	- `ssh dokku@SERVERNAME docker-options:add gt1-ui run,deploy --volume /var/lib/dokku/services/infobright/gt1-db/import:/import` stelle einen beschreibbaren Order im Hostsystem für die Dateiübergabe an die Datenbank zur Verfügung
	- `ssh dokku@SERVERNAME docker-options:add gt1-ui run,deploy --volume /var/upload/gt1/DATA_GTI/Downloaded_ProcessedData:/data:ro` Zugriff auf die zu importierenden Daten durch die Anwendung, nur lesender Zugriff
    - `git push production` Kopiere Quellcode der Anwendung auf den Produktivserver, dokku baut und startet die Anwendung
    - `ssh dokku@SERVERNAME docker-options:add infobright deploy "-v /home/dokku/IMPORT_PFAD:/import"`
    - `ssh dokku@SERVERNAME docker-options:add infobright deploy "-e MYSQL_ROOT_PASSWORD=<....>"`
    - `git push production`
  
# Betrieb
Im laufenden Betrieb sollten auf dem Server immer zwei Container laufen: Die Anwendung selbst und die Datenbank. Sollte die Anwendung nicht laufen kann sie mit `dokku ps:restart gt1-ui` auf dem Server neu gestartet werden. Logausgaben sind mit `dokku logs gt1-ui` sichtbar, Zugriffslogs von NGINX per `dokku nginx:access-logs gt1-ui`. Weitere Befehle sind in der Dokumentation von [dokku](http://dokku.viewdocs.io/dokku/) nachzulesen.
