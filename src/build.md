To clear all packages locally to test, use the following code within R:

```{r}
ip <- as.data.frame(installed.packages())
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
path.lib <- unique(ip$LibPath)
pkgs.to.remove <- ip[,1]
sapply(pkgs.to.remove, remove.packages, lib = path.lib)
```

To build the materials zip file, do the following in a terminal:

```{sh}
rm -rf materials
mkdir materials
mkdir materials/data
mkdir materials/data_project
mkdir materials/funs
mkdir materials/notebooks
mkdir materials/output

cp notebooks/project.Rproj materials/notebooks
cp notebooks/setup.Rmd materials/notebooks
cp notebooks/notebook00.Rmd materials/notebooks
cp funs/funs.R materials/funs
cp funs/update.R materials/funs

rm -f materials.zip
zip -r materials.zip materials
rm -rf materials
mv materials.zip extra
```
