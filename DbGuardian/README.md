We are using [[https://www.atlassian.com/git/tutorials/git-subtree][git-subtree]] to get FDW from external Git repositories.

In particular, this is how it is done for each foreign data wrapper:

> git remote add -f mysql_fdw https://github.com/EnterpriseDB/mysql_fdw.git
> git subtree add --prefix DbGuardian/foreign_data_wrappers/mysql_fdw mysql_fdw master --squash
and then:
> git subtree pull --prefix DbGuardian/foreign_data_wrappers/mysql_fdw mysql_fdw master --squash

> git remote add -f tds_fdw https://github.com/tds-fdw/tds_fdw.git
> git subtree add --prefix DbGuardian/foreign_data_wrappers/tds_fdw tds_fdw master --squash
and then:
> git subtree pull --prefix DbGuardian/foreign_data_wrappers/tds_fdw tds_fdw master --squash


> git remote add -f oracle_fdw https://github.com/laurenz/oracle_fdw.git
> git subtree add --prefix DbGuardian/foreign_data_wrappers/oracle_fdw oracle_fdw master --squash
and then:
> git subtree pull --prefix DbGuardian/foreign_data_wrappers/oracle_fdw oracle_fdw master --squash 
