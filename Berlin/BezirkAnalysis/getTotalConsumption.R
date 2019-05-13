getTotalConsumption <- function(totalArea,spz_verbrauch_mean,gtype) {
  if (gtype=="SFH") {
    totalConsumption <- data.frame(abrechnungsjahr = 2002:2018,gesamtVerbrauch = totalArea$areaSFH*spz_verbrauch_mean$mean_spzverbrauch)
  }
  if (gtype=="MFH") {
    totalConsumption <- data.frame(abrechnungsjahr = 2002:2018,gesamtVerbrauch = totalArea$areaMFH*spz_verbrauch_mean$mean_spzverbrauch)
  }
  return(totalConsumption)
}