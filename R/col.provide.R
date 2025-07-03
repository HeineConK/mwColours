.as.rgb <- function(ra, ga, ba, complement = NULL){

  if(is.null(complement)) rgb( ra / 255, ga / 255, ba / 255 )
  else if(complement == "red"){ rgb( ba / 255, ra / 255, ga / 255 ) }
  else if(complement == "green") rgb(ga / 255 , ba / 255, ra / 255)
}

.col.list <- list(
  # HSMW blues: ----

  # Default blue as used in the logo and main text
  blue = .as.rgb(0, 105, 179),

  # Blues used for categories ---
  blue1 = .as.rgb(0, 85, 147),
  blue2 = .as.rgb(0, 105, 179), # This is actually the same as the default blue
  blue3 = .as.rgb(160, 177, 210),

  # 'Unofficial' complementary reds used for categories ---
  red  = .as.rgb(0, 105, 179, complement = "red"), # actually the same as red2; implemented only for consistency sake
  red1 = .as.rgb(0, 85, 147, complement = "red"),
  red2 = .as.rgb(0, 105, 179, complement = "red"),
  red3 = .as.rgb(160, 177, 210, complement = "red"),

  # 'Unofficial' complementary greens used for categories ---
  green  = .as.rgb(0, 105, 179, complement = "green"), # actually the same as green2; implemented only for consistency sake
  green1 = .as.rgb(0, 85, 147, complement = "green"),
  green2 = .as.rgb(0, 105, 179, complement = "green"),
  green3 = .as.rgb(160, 177, 210, complement = "green"),

  # Subtext gray: ----
  gray = .as.rgb(166, 166, 166),

  # Department colors as given on the website:

  es =.as.rgb(6, 41, 81),  # engineering sciences
  cb =.as.rgb(233, 120, 23),  # appl. comp and bio sciences
  ie =.as.rgb(0, 117, 61),  # industrial engineering
  ss =.as.rgb(152, 14, 92),  # social sciences
  me =.as.rgb(88, 161, 44),  # media
  inst =.as.rgb(42, 107, 183)  # associated institutes

)

#' Shows the UAS Mittweida colour set
#'
#' A simple graphical output listing all of the colours provided by this package. Please note that
#' UAS red and green colours are simple RGB complementaries to
#' the official blues and are not officially part of the corporate design.
#' However these reds and greens might be used for nonofficial purposes in cases
#' where showing categorical data requires more contrast and colour levels than supported by
#' the three CD blues, which would be more appropriate anyway.
#' @export show_colors
#'
#'
show_colours <- function(){

  nc <- length(.col.list)


  plot(NULL, type="n", xaxt = "n", xlim = c(0,2), ylim = c(0,(nc+1)),
       frame.plot = F, yaxt = "n", xlab = "", ylab = "")


  rect(xleft = 1, xright = 2, ybottom  = 1:nc, ytop  = 2:(nc+1), border = F, col = rev(unlist(.col.list)))


  text(1, 1:nc + .5, rev(names(.col.list)),pos=2)
}


#' A helper function for changing the transparency value of a given colour.
#'
#' Actually not intended to be called directly.
#'
#' @export acol
#'
#'
acol <- function (acol, alpha = 0.2){
  if(length(acol) > 1){
    sapply(acol, function(ac){
      ac <- col2rgb(ac)
      ac <- rgb(ac[1]/255, ac[2]/255, ac[3]/255, alpha)
      ac
    })
  }
  else{
    acol <- col2rgb(acol)
    acol <- rgb(acol[1]/255, acol[2]/255, acol[3]/255, alpha)
    acol
  }
}

#' Returns the list of valid colour names.
#'
#' @export col_names
#'
#'
col_names <- function(){
  names( .col.list )
}

#' Returns a named list of all colours; or a data frame containing names, hex codes and RGB values.
#'
#' @param return.df logical. Set to TRUE if a data frame should be returned. Defaults to FALSE.
#'
#' @export col_palette
#'
#'
col_palette <- function(return.df = FALSE){
  if(return.df){
    rgbs <- sapply(.col.list, col2rgb)
    cdf <- data.frame(
      name = names( .col.list ),
      hex  = unlist( .col.list ),
      red = rgbs[1,],
      green = rgbs[2,],
      blue = rgbs[3,]
    )
    rownames(cdf) <- NULL
    return( cdf )
  }

  .col.list
}

#' Returns the colour value given a valid colour name.
#'
#' @param cname Colour name. See col_names for a list of valid names. Returns default blue if no name is specified.
#' @param alpha Optional transparency value. Set to 1 by default.
#' @param return.rgb logical. Use when RGB values should be returned instead of hex value.
#' @export get_col
#'
#'
get_col <- function( cname = NULL, alpha = 1, return.rgb = FALSE ){
  pc <- NULL

  if(is.null(cname)){
    pc <- .col.list[[ "blue" ]]
  }else{
   is.name.valid <- cname %in% names(.col.list)

   if(is.name.valid){
     pc <- .col.list[[ cname ]]
   }else{
     warning("Invalid colour name specified. Returning default. See ?col_names for a list of valid names.")
     pc <- .col.list[[ "hsmw.blue" ]]
   }
  }

  if(alpha != 1){
    if(alpha > 0 && alpha < 1){
      pc <- acol( pc, alpha  )
    }else{
      warning("Invalid alpha value specified")
    }
  }

  if(return.rgb){
    rgb <- col2rgb(pc, alpha = T)
    pc <- as.vector(rgb)
    names(pc) <- rownames(rgb)
  }

  return( pc )
}
