package com.raphtory.RawGraphsModels

case class GabPost (
  id : Long,
  created_at : String,
  revised_at : String,
  edit : Boolean,
  body : String,
  body_html : String,
  body_html_summary : String,
  body_html_summary_trucated : Boolean,
  only_emojy : Boolean,
  liked : Boolean,
  disliked : Boolean,
  bookmarked : Boolean,
  repost : Boolean,
  score : Int,
  like_count : Int,
  dislike_count : Int,
  reply_count : Int,
  repost_count : Int,
  is_quote : Boolean,
  is_reply : Boolean,
  is_replies_disabled : Boolean,
  //embed : GabEmbed,
  //attachment : GabAttachment,
  category : Int,
  categoryDetails: GabCategoryDetails,
  language : String,
  nsfw : Boolean,
  is_premium : Boolean,
  is_locked : Boolean,
  user : GabUser,
  topic : GabTopic,
)

case class GabEmbed(
  html : Option[String],
  iframe : Option[String]
)

case class GabAttachment(
  type_ : String,
  value : Seq[GabMedia]
)

case class GabMedia(
  id : String,
  url_thumbnail : String,
  url_full : String,
  width : Int,
  height : Int
  )
case class GabCategoryDetails(
  title : String,
  slug : String,
  value : Int,
  emoji : String
)

case class GabUser(
  id : Int,
  name : String,
  username : String,
  picture_url : String,
  verified : Boolean,
  is_donor : Boolean,
  is_investor : Boolean,
  is_pro : Boolean,
  is_private : Boolean,
  is_premium : Boolean
)

case class GabTopic (
  id : String,
  created_at : String,
  is_featured : Boolean,
  title : String,
  category : Long
)

case class GabReplies (
  data : String
)
