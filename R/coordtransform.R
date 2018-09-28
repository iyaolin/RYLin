#算法来源：https://github.com/geosmart/coordtransform/blob/master/src/main/java/me/demo/util/geo/CoordinateTransformUtil.java

#百度坐标(BD09)、国测局坐标(火星坐标,GCJ02)、和WGS84坐标系互转
pi = 3.1415926535897932384626 # 圆周率
a = 6378245.0  # 长半轴
ee = 0.00669342162296594323  # 扁率

x_pi = 3.14159265358979324 * 3000.0 / 180.0


transformlat<-function(lng,lat){
  ret =(-100.0) + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret =ret+ (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lat * pi) + 40.0 *sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret =ret+ (160.0 * sin(lat / 12.0 * pi) + 320 *sin(lat * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}

transformlng<-function(lng,lat){
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret =ret+ (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lng * pi) + 40.0 *sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret =ret+ (150.0 * sin(lng / 12.0 * pi) + 300.0 *sin(lng / 30.0 * pi)) * 2.0 / 3.0
  return(ret)
}

WGS84toGCJ02<-function(lng,lat){
  #WGS84转GCJ02(火星坐标系)
  #:param lng:WGS84坐标系的经度
  #:param lat:WGS84坐标系的纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  # return(paste(as.character(mglng),as.character(mglat)))
  return(cbind(mglng, mglat))
}

GCJ02toWGS84<-function(lng,lat){
  #GCJ02(火星坐标系)转GPS84
  #:param lng:火星坐标系的经度
  #:param lat:火星坐标系纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  lng_result=lng * 2 - mglng
  lat_result=lat * 2 - mglat
  # return(list(lng=lng_result, lat=lat_result))
  return(cbind(lng_result, lat_result))
}

#下面的函数，用于单独输出经纬度
WGS84toGCJ02_lng<-function(lng,lat){
  #WGS84转GCJ02(火星坐标系)
  #:param lng:WGS84坐标系的经度
  #:param lat:WGS84坐标系的纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return(mglng)
}

WGS84toGCJ02_lat<-function(lng,lat){
  #WGS84转GCJ02(火星坐标系)
  #:param lng:WGS84坐标系的经度
  #:param lat:WGS84坐标系的纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return(mglat)
}

GCJ02toWGS84_lng<-function(lng,lat){
  #GCJ02(火星坐标系)转GPS84
  #:param lng:火星坐标系的经度
  #:param lat:火星坐标系纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  lng_result=lng * 2 - mglng
  lat_result=lat * 2 - mglat
  return (lng_result)
}
GCJ02toWGS84_lat<-function(lng,lat){
  #GCJ02(火星坐标系)转GPS84
  #:param lng:火星坐标系的经度
  #:param lat:火星坐标系纬度
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  lng_result=lng * 2 - mglng
  lat_result=lat * 2 - mglat
  return (lat_result)
}



  # /**
  #  * 百度坐标系(BD-09)转火星坐标系(GCJ-02)
  #  * 
  #  * @see 百度——>谷歌、高德
  #  * @param lng 百度坐标纬度
  #  * @param lat 百度坐标经度
  #  * @return 火星坐标数组
  #  */
BD09toGCJ02 <- function(lng, lat){
  x = lng - 0.0065
  y = lat - 0.006
  z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lng = z * cos(theta)
  gg_lat = z * sin(theta)
  return(cbind(gg_lng, gg_lat))
}

  # /**
  #  * 火星坐标系(GCJ-02)转百度坐标系(BD-09)
  #  * 
  #  * @see 谷歌、高德——>百度
  #  * @param lng 火星坐标经度
  #  * @param lat 火星坐标纬度
  #  * @return 百度坐标数组
  #  */
GCJ02toBD09 <- function(lng, lat){
    z = sqrt(lng * lng + lat * lat) + 0.00002 * sin(lat * x_pi)
    theta = atan2(lat, lng) + 0.000003 * cos(lng * x_pi)
    bd_lng = z * cos(theta) + 0.0065
    bd_lat = z * sin(theta) + 0.006
    return (cbind(bd_lng, bd_lat))
}


  # /**
  #  * 百度坐标系(BD-09)转WGS坐标
  #  * 
  #  * @param lng 百度坐标纬度
  #  * @param lat 百度坐标经度
  #  * @return WGS84坐标数组
  #  */
BD09toWGS84 <- function(lng, lat){
  gcj = BD09toGCJ02(lng, lat)
  wgs84 = GCJ02toWGS84(gcj[, 1], gcj[, 2])
  return(wgs84)
}

  # /**
  #  * WGS坐标转百度坐标系(BD-09)
  #  * 
  #  * @param lng WGS84坐标系的经度
  #  * @param lat WGS84坐标系的纬度
  #  * @return 百度坐标数组
  #  */
WGS84toBD09 <- function(lng, lat){
  gcj = WGS84toGCJ02(lng, lat)
  bd09 = GCJ02toBD09(gcj[, 1], gcj[, 2])
  return(bd09)
}


