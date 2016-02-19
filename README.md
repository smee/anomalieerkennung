# Production
Auf bisfue.uni-leipzig.de

- Installiere Infobrightcontainer von https://github.com/meatcar/docker-infobright
- Konfiguriere:
  - `git remote add production dokku@bisfue.uni-leipzig.de:infobright`
  - `git push production`
  - ssh auf bisfue, `su dokku`, `cd && mkdir infobright/data && mkdir infobright/import`
  - `ssh dokku@bisfue docker-options:add infobright deploy "-v /home/dokku/infobright/import:/import"`
  - `ssh dokku@bisfue docker-options:add infobright deploy "-v /home/dokku/infobright/data:/mnt/mysql_data"`
  - `ssh dokku@bisfue docker-options:add infobright deploy "-e MYSQL_ROOT_PASSWORD=<....>"`
  - `git push production`
  
