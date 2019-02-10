## A function to create a KML file that can be appended with data ##
NONAME.track2kmlOpenFile <- function(output.filename = "track", descriptive.filename = NULL)
  {
  if (is.null(descriptive.filename)) {
    descriptive.filename <- output.filename
  }
  filename <- paste(output.filename, ".kml", sep = "")
  write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", filename)
  write("<kml xmlns=\"http://www.opengis.net/kml/2.2\">", filename, 
        append = TRUE)
  write("<Document>", filename, append = TRUE)
  write(paste("<name>", descriptive.filename, "</name>", sep = " "), 
        filename, append = TRUE)
  write("  <open>1</open>", filename, append = TRUE)
  
}

## A function to append a folder to the KML
NONAME.track2kmlAddFolder <- function(output.filename = "track", descriptive.foldername = NULL)
{
  filename <- paste(output.filename, ".kml", sep = "")
  write("<Folder>", filename, append = TRUE)
  write(paste("<name>", descriptive.foldername, "</name>", sep = " "), 
        filename, append = TRUE)
  write("  <open>1</open>", filename, append = TRUE)
}

## A function to append a folder to the KML
NONAME.track2kmlCloseFolder <- function(output.filename = "track")
{
  filename <- paste(output.filename, ".kml", sep = "")
  write("</Folder>", filename, append = TRUE) #closes the overarching folder
}

## A function to append KML data into folders ##
## Output in meters ##
NONAME.track2kmlAppendData <- function (latitude, longitude, datetime, altitude = NULL, col.variable = NULL, 
    col.scheme = NULL, point.alpha = 255, line.color = "goldenrod", 
    line.alpha = 255, size.variable = NULL, 
    data.variables = NULL, output.filename = "track",descriptive.foldername= "Look") 
{
  filename <- paste(output.filename, ".kml", sep = "")
  require(RColorBrewer)
  if (is.null(col.variable) & is.null(col.scheme)) {
    col.scheme <- "magenta"
  }
  if (!is.null(col.variable) & is.null(col.scheme)) {
    stop("Specify a col.scheme or set col.variable to NULL.")
  }
  if (length(col.scheme) == 1 && (col.scheme %in% colors() | 
                                  is.numeric(col.scheme) | strsplit(col.scheme[1], split = "")[[1]][1] == 
                                  "#")) {
    usable.colors <- strsplit(rep(rgb(col2rgb(col.scheme)[1, 
                                                          1], col2rgb(col.scheme)[2, 1], col2rgb(col.scheme)[3, 
                                                                                                             1], point.alpha, maxColorValue = 255), length(latitude)), 
                              split = "")
  }
  else if (length(col.scheme) == length(latitude)) {
    usable.colors <- strsplit(rgb(col2rgb(col.scheme)[1, 
                                                      ], col2rgb(col.scheme)[2, ], col2rgb(col.scheme)[3, 
                                                                                                       ], point.alpha, maxColorValue = 255), split = "")
  }
  else if (!is.null(col.variable) & col.scheme %in% row.names(RColorBrewer::brewer.pal.info)) {
    colors <- strsplit(rev(RColorBrewer::brewer.pal(9, col.scheme)), 
                       split = "")
    ratio <- (max(col.variable) - min(col.variable))/length(colors)
    brks <- round(seq(min(col.variable), max(col.variable), 
                      by = ratio), digits = 0)
    colorIndx <- findInterval(col.variable, brks, all.inside = TRUE)
    usable.colors <- lapply(colors[colorIndx], FUN = function(x) c(x, 
                                                                   unlist(strsplit(rgb(0, 0, 0, point.alpha, maxColorValue = 255), 
                                                                                   split = ""))[c(8, 9)]))
  }
  else if (!is.null(col.variable) & col.scheme[1] %in% c("rainbow", 
                                                         "heat.colors", "terrain.colors", "topo.colors", "cm.colors", 
                                                         "bpy.colors")) {
    if (col.scheme == "bpy.colors") {
      require(sp)
    }
    colors <- strsplit(eval(parse(text = paste(col.scheme, 
                                               "(ifelse(length(latitude) < 1000, length(latitude), 1000), alpha=point.alpha/255)", 
                                               sep = ""))), split = "")
    ratio <- (max(col.variable, na.rm = TRUE) - min(col.variable, 
                                                    na.rm = TRUE))/length(colors)
    brks <- round(seq(min(col.variable, na.rm = TRUE), max(col.variable, 
                                                           na.rm = TRUE), by = ratio), digits = 0)
    colorIndx <- findInterval(col.variable, brks, all.inside = TRUE)
    usable.colors <- colors[colorIndx]
  }
  else {
    stop("The col.scheme has been misspecified or no col.variable was given.")
  }
  usable.line.color <- strsplit(rgb(col2rgb(line.color)[1, 
                                                        1], col2rgb(line.color)[2, 1], col2rgb(line.color)[3, 
                                                                                                           1], col2rgb(line.color, alpha = line.alpha)[4, 1], maxColorValue = 255), 
                                split = "")
  date <- unlist(strsplit(as.character(datetime), split = " "))[seq(1, 
                                                                    ((length(datetime) * 2) - 1), by = 2)]
  time <- unlist(strsplit(as.character(datetime), split = " "))[seq(2, 
                                                                    ((length(datetime) * 2)), by = 2)]
  if (!is.null(size.variable)) {
    scaling.parameter <- log1p(size.variable + abs(min(size.variable, 
                                                       na.rm = TRUE)))/max(log1p(size.variable + abs(min(size.variable, 
                                                                                                         na.rm = TRUE))), na.rm = TRUE)
  }
  else {
    scaling.parameter <- rep(pointsize, length(latitude))
  }
  if (any(is.na(scaling.parameter))) {
    scaling.parameter[which(is.na(scaling.parameter) == TRUE)] <- rep(min(scaling.parameter, 
                                                                          na.rm = TRUE), sum(is.na(scaling.parameter)))
    warning("Missing values included in 'size.variable' assigned smallest size.")
  }
  
    write("<Folder>", filename, append = TRUE)
    write(paste("<name>", descriptive.foldername, "</name>", sep = " "), 
        filename, append = TRUE)
    write("  <open>1</open>", filename, append = TRUE)
    write("<Folder>", filename, append = TRUE)
    write("  <name>Points</name>", filename, append = TRUE)
    write("<open>0</open>", filename, append = TRUE)
    for (i in 1:length(latitude)) {
      ##
      ##need to add a name of the placemarks (showing up as noname)
        write("<Placemark id='point'>", filename, append = TRUE)
        write(paste("<name>", "</name>", sep = " "), # removed time[i]
            filename, append = TRUE)
        write("  <TimeSpan>", filename, append = TRUE)
        write(paste("    <begin>", date[i], "T", time[i], "Z</begin>", 
            sep = ""), filename, append = TRUE)
        write(paste("    <end>", date[ifelse(i == length(latitude), 
            i, i + 1)], "T", time[ifelse(i == length(latitude), 
            i, i + 1)], "Z</end>", sep = ""), filename, append = TRUE)
        write("  </TimeSpan>", filename, append = TRUE)
        write("<visibility>1</visibility>", filename, append = TRUE)
        if (!is.null(data.variables)) {
            extra.data.text <- c()
            for (j in 1:length(data.variables)) {
                extra.data.text <- append(extra.data.text, paste("<TR><TD>", 
                  names(data.variables[j]), "</TD><TD>", data.variables[i, 
                    j], "</TD></TR>", sep = ""))
            }
            all.extra.data.text <- paste(extra.data.text, sep = "", 
                collapse = "")
            write("<description>", filename, append = TRUE)
            write(paste("<![CDATA[<TABLE border='1'><TR><TD><B>Variable</B></TD><TD><B>Value</B></TD></TR><TR><TD>Date/Time</TD><TD>", 
                datetime[i], "</TD></TR><TR><TD>lat long</TD><TD>", 
                paste(latitude[i], longitude[i], sep = " "), 
                "</TD></TR>", all.extra.data.text, "</TABLE>]]>", 
                sep = "", collapse = ""), filename, append = TRUE)
            write("</description>", filename, append = TRUE)
        }
        else {
            write("<description>", filename, append = TRUE)
            write(paste("<![CDATA[<TABLE border='1'><TR><TD><B>Variable</B></TD><TD><B>Value</B></TD></TR><TR><TD>Date/Time</TD><TD>", 
                datetime[i], "</TD></TR><TR><TD>lat long</TD><TD>", 
                paste(latitude[i], longitude[i], sep = " "), 
                "</TABLE>]]>", sep = "", collapse = ""), filename, 
                append = TRUE)
            write("</description>", filename, append = TRUE)
        }
        write("\t<Style>", filename, append = TRUE)
        write("\t<IconStyle>", filename, append = TRUE)
        write(paste("\t\t<color>", paste(noquote(usable.colors[[i]][c(8, 
            9, 6, 7, 4, 5, 2, 3)]), collapse = ""), "</color>", 
            sep = ""), filename, append = TRUE)
        write(paste("  <scale>", scaling.parameter[i], "</scale>", 
            sep = ""), filename, append = TRUE)
        write("\t<Icon>", filename, append = TRUE)
        write("\t\t<href>http://maps.google.com/mapfiles/kml/pal2/icon26.png</href>", 
            filename, append = TRUE)
        write("\t</Icon>", filename, append = TRUE)
        write("\t</IconStyle>", filename, append = TRUE)
        write("\t</Style>", filename, append = TRUE)
        write("\t<Point>", filename, append = TRUE)
        write(paste("\t<altitudeMode>", ifelse(is.null(altitude[i]), 
            "relativeToGround", "absolute"), "</altitudeMode>", 
            sep = ""), filename, append = TRUE)
        write("<tesselate>1</tesselate>", filename, append = TRUE)
        write("<extrude>1</extrude>", filename, append = TRUE)
        write(paste("\t  <coordinates>", longitude[i], ",", latitude[i], 
            ",", ifelse(is.null(altitude[i]), 1, altitude[i]), 
            "</coordinates>", sep = ""), filename, append = TRUE)
        write("\t</Point>", filename, append = TRUE)
        write(" </Placemark>", filename, append = TRUE)
    }
    write("</Folder>", filename, append = TRUE)
    write("<Placemark>", filename, append = TRUE)
    write("  <name>Line Path</name>", filename, append = TRUE)
    write("  <Style>", filename, append = TRUE)
    write("    <LineStyle>", filename, append = TRUE)
    write(paste("\t<color>", paste(noquote(usable.line.color[[1]][c(8, 
        9, 6, 7, 4, 5, 2, 3)]), collapse = ""), "</color>", sep = ""), 
        filename, append = TRUE)
    write(paste("      <width>1</width>", sep = ""), filename, 
        append = TRUE)
    write("    </LineStyle>", filename, append = TRUE)
    write("  </Style>", filename, append = TRUE)
    write("  <LineString>", filename, append = TRUE)
    write("    <extrude>0</extrude>", filename, append = TRUE)
    write("    <tessellate>1</tessellate>", filename, append = TRUE)
    write(paste("\t<altitudeMode>absolute</altitudeMode>", 
        sep = ""), filename, append = TRUE)
    write(paste("     <coordinates>", noquote(paste(longitude, 
        ",", latitude, sep = "", collapse = " ")), "</coordinates>", 
        sep = ""), filename, append = TRUE)
    write("    </LineString>", filename, append = TRUE)
    write("</Placemark>", filename, append = TRUE)
    write("</Folder>", filename, append = TRUE) #closes the overarching folder
}

## A function to create a KML file that can be appended with data ##
NONAME.track2kmlCloseFile <- function(output.filename="track")
{
  filename <- paste(output.filename, ".kml", sep = "")
  write("</Document>", filename, append = TRUE)
  write("</kml>", filename, append = TRUE)
  
}
