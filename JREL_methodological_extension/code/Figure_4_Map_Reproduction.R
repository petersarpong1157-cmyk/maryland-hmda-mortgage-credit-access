# Recreate Figure 4 from the verified Maryland HMDA CSV and 2025 Census boundaries.
# This script is a documented reconstruction of the interactive R workflow.
# Run locally; map regeneration has not yet been verified from a fresh session.
# Input is intentionally excluded from GitHub; original public HMDA data required.
#
# Required CRAN packages: readr, dplyr, sf, ggplot2, viridisLite,
# patchwork, ggspatial, svglite.
#
# Usage: from the directory containing loan_purposes_1_state_MD.csv:
# source("JREL_methodological_extension/code/Figure_4_Map_Reproduction.R")

pkgs <- c("readr","dplyr","sf","ggplot2","viridisLite",
          "patchwork","ggspatial","svglite")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Install missing packages: ", paste(missing,collapse=", "))
input <- "loan_purposes_1_state_MD.csv"
if (!file.exists(input)) stop("Missing source CSV: ", input)
expected <- "bfc55246d7307f2f2e44ba4a5e7c12b8"
if (!identical(unname(tools::md5sum(input)), expected)) {
  stop("Source CSV MD5 does not match the validated Maryland extract.")
}
outdir <- "JREL_map_outputs"
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)
x <- readr::read_csv(input, col_types=readr::cols(.default=readr::col_character()),
                     show_col_types=FALSE)
r <- x |>
  dplyr::filter(action_taken %in% c("1","2","3"),
                occupancy_type=="1", construction_method=="1",
                lien_status=="1",
                derived_dwelling_category=="Single Family (1-4 Units):Site-Built",
                total_units=="1") |>
  dplyr::mutate(GEOID=trimws(census_tract),
                minority_pct=suppressWarnings(as.numeric(tract_minority_population_percent)))
stopifnot(nrow(r)==60707)
valid <- r |>
  dplyr::filter(grepl("^24[0-9]{9}$", GEOID),
                !is.na(minority_pct), minority_pct>=0, minority_pct<=100)
stopifnot(nrow(valid)==60663)
tract <- valid |>
  dplyr::group_by(GEOID) |>
  dplyr::summarise(applications=dplyr::n(), minority_min=min(minority_pct),
                   minority_max=max(minority_pct),
                   minority_pct=stats::median(minority_pct), .groups="drop")
stopifnot(nrow(tract)==1433, all(tract$minority_max-tract$minority_min <= .01))
url <- "https://www2.census.gov/geo/tiger/TIGER2025/TRACT/tl_2025_24_tract.zip"
z <- file.path(outdir,"tl_2025_24_tract.zip")
if (!file.exists(z)) utils::download.file(url,z,mode="wb")
shape_dir <- file.path(outdir,"census_boundaries")
dir.create(shape_dir,recursive=TRUE,showWarnings=FALSE)
utils::unzip(z,exdir=shape_dir)
shape <- list.files(shape_dir,pattern="\\.shp$",full.names=TRUE)
stopifnot(length(shape)==1)
md <- sf::st_read(shape,quiet=TRUE)
stopifnot("GEOID" %in% names(md))
stopifnot(length(setdiff(tract$GEOID,md$GEOID))==0)
mapped <- dplyr::left_join(md,tract,by="GEOID") |> sf::st_transform(5070)
breaks <- c(0,20,40,60,80,100)
labs <- c("0–20%","20–40%","40–60%","60–80%","80–100%")
mapped$minority_group <- cut(mapped$minority_pct,breaks=breaks,labels=labs,include.lowest=TRUE)
stopifnot(sum(!is.na(mapped$minority_group))==1433)
mapped$map_category <- factor(ifelse(is.na(mapped$minority_group),
                                    "Not represented",as.character(mapped$minority_group)),
                              levels=c(labs,"Not represented"))
colors <- c(stats::setNames(rev(viridisLite::viridis(5)),labs),
            "Not represented"="#b8bdc3")
crop_region <- function(g,xmin,ymin,xmax,ymax) {
  b <- sf::st_bbox(c(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),crs=sf::st_crs(4326))
  b <- sf::st_transform(sf::st_as_sfc(b),sf::st_crs(g))
  sf::st_crop(g,sf::st_bbox(b))
}
panel <- function(g,ttl,legend=FALSE,scale=FALSE) {
  p <- ggplot2::ggplot(g)+ggplot2::geom_sf(ggplot2::aes(fill=map_category),color=NA)+
    ggplot2::scale_fill_manual(values=colors,drop=FALSE,name="Minority population share")+
    ggplot2::coord_sf(datum=NA)+ggplot2::labs(title=ttl)+
    ggplot2::theme_void(base_size=13)+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=15),
      legend.position=if(legend) "bottom" else "none",
      legend.text=ggplot2::element_text(size=10),
      legend.title=ggplot2::element_text(size=11,face="bold"),
      plot.margin=ggplot2::margin(8,8,8,8))
  if(scale) p <- p+ggspatial::annotation_scale(location="bl",width_hint=.20)+
    ggspatial::annotation_north_arrow(location="tr",which_north="true",
                                      style=ggspatial::north_arrow_minimal)
  p
}
balt <- crop_region(mapped,-76.95,39.10,-76.30,39.55)
dc <- crop_region(mapped,-77.45,38.65,-76.65,39.25)
fig <- (panel(mapped,"A. Maryland statewide",TRUE,TRUE) /
        (panel(balt,"B. Baltimore metropolitan area") |
         panel(dc,"C. Washington suburban area")))+
  patchwork::plot_layout(heights=c(1.1,1.3))+
  patchwork::plot_annotation(
    title="Neighborhood Minority Composition Across Maryland",
    subtitle="2025 HMDA restricted sample | 1,433 mapped census tracts",
    caption=paste("Source: 2025 Maryland HMDA public data; 2025 U.S. Census",
                  "TIGER/Line boundaries. Gray indicates Maryland tracts",
                  "not represented in the mapped sample."))
ggplot2::ggsave(file.path(outdir,"Figure_4_Maryland_Map_Final.png"),
                fig,width=13,height=11,dpi=600,bg="white")
ggplot2::ggsave(file.path(outdir,"Figure_4_Maryland_Map_Final.svg"),
                fig,width=13,height=11,device=svglite::svglite,bg="white")
readr::write_csv(tract,file.path(outdir,"Figure_4_Tract_Data.csv"))
message("Figure 4 regenerated; 1,433 matched tracts and 42 not represented.")
