-include_lib("cocktail/include/ctail.hrl").

-ifndef(IM_ENTITIES_HRL).
-define(IM_ENTITIES_HRL, true).

-record(im_usr, {
  id,
  name,
  username,
  photo,
  thumbnail,
  email,
  skype,
  phone,
  bio,
  sendBioToNewContacts=false,
  facebookId,
  vkontakteId,
  departmentId,
  createdAt=0,
  updatedAt=0,
  active=true,
  excludeMe=false,
  spamCount=0,
  isNew=true,     %% if true send ContactsRegisteredEvent after update main profile
  isBot=false,
  isVendor=false, %% true if its vendor's main account
  vendorId,       %% not empty if its vendor representative
  roles=[],
  data,           %% json encoded string of user metadata
  roster=[],
  rooms=[],
  chatUpdates=[],
  devices=[],
  blocks=[],
  lastSeen=0,
  contactUserIds=[]
}).

-record(im_usr_role, {id, name, permissions=[]}).
-record(im_usr_perm, {id, name}).

-record(im_usr_token, {
  id,
  userId,
  os,
  deviceId,
  deviceName,
  locale,
  expired=0
}).

-record(im_usr_phone, {id, userId}).

-record(im_code, {id, code, expired=0}).

-record(im_grp, {
  id,
  createdBy,
  topic,
  picture,
  thumbnail,
  created=0,
  updated=0,
  members=[],
  admins=[],
  deleted=false,
  requestId,
  circleParams
}).

-record(im_msg, {
  ?ITERATOR(feed),
  type,
  created,
  userTime,
  updated,
  systemMessageType,
  systemMessageParams,
  recipientOnly,
  recipient,
  origin,
  payload,
  media=[],
  starred,
  geo=[],
  kind=0,
  inlineKeyboardMarkup,
  deleted,
  deletedBy=[],
  deletedByOwner,
  replyId,
  forwardId,
  originalId,
  callId,
  isEdited=false
}).

-record(im_msg_star, {id, messageIds}).

-record(im_chat_update, {
  id                    :: string(),
  timestamp=0           :: integer(),
  top                   :: string(),
  delivered=0           :: integer(),
  seen=0                :: integer(),
  lastSendSeen=0        :: integer(),
  unread=0              :: integer(),
  hide=false            :: boolean(),
  name                  :: string(),
  thumbnail             :: string(),
  deleted=false         :: boolean(),
  replyKeyboardMarkup   :: tuple(),
  removeKeyboardMarkup  :: tuple(),
  callId                :: string()
}).

-record(im_update, {
  ?ITERATOR(feed),
  type,
  feedType,
  feedId,
  created,
  messageId,
  taskId,
  ids=[]
}).

-record(im_muc_history_marker, {
  id,
  time
}).

-record(im_csr, {
  ?ITERATOR(feed),
  userId,
  vendorId,
  status,
  roomId,
  descr,
  thumbnail,
  created,
  updated,
  activity=[],
  grabbedBy,
  staff=[],
  tags=[]
}).

-record(im_bot, {
  id,
  type,
  userId,
  username,
  descr,
  accessToken,
  webhookUrl,
  commands=[],
  isInline=false,
  isSystem=false,
  updated
}).

-record(im_bot_server_msg, {
  ?ITERATOR(feed),
  action,
  jsonData
}).

-record(im_bot_user_lookup, {id, botId}).
-record(im_bot_username_lookup, {id, botId}).

-record(im_device, {id, token, os, name, tokens=[]}).
-record(im_device_push_token, {id, os, type}).
-record(im_device_locale_lookup, {id, locale}).
-record(im_device_push_token_lookup, {id, userId}).

-record(im_department, {id, name}).

-record(im_directory, {
  id,
  userId,
  name,
  email,
  phone,
  photo,
  thumbnail,
  departmentId,
  isNew=true,
  blocked=false
}).

-record(im_task, {
  id,
  reporter,
  assignee,
  status,
  payload,
  media=[],
  created,
  updated,
  sourceMessageIds=[],
  feedId,
  deadline
}).

-record(im_call, {
  ?ITERATOR(feed),
  callId,
  idMap=[],
  feedType,
  feedId,
  initiatorId,
  created,
  duration,
  direction,
  participated=[],
  status
}).

-record(im_call_user_lookup, {id, feedCallId}). %% id - {callId, userId}, feedCallId - #im_call.id

-record(im_file, {
  id,
  path,
  name,
  mime,
  thumbnail,
  size,
  width,
  height,
  duration
}).

-record(im_lookup_file, {hash, fileId, expires}).

-record(im_feed_post_category, {
  id,
  tag,
  created,
  updated,
  name,
  parentId,
  thumbnail,
  isFeatured=false
}).

-record(im_feed_post, {
  id,
  type,
  tags=[],
  categories=[],
  parentId,
  title,
  descr,
  phone,
  payload,
  author,
  thumbnail,
  buttonCaption,
  targetLink,
  location,
  media=[],
  relations=[],
  address,
  workHours,
  userId,
  paymentInfo,
  created,
  updated,
  date,
  vendorId,
  availableDates=[]
}).

-record(im_like, {
  id,
  recordType,
  recordId,
  type,
  userId,
  created
}).

-record(im_like_count, {id, count=0}).

-record(im_localized_store, {id, name, location}).

-record(im_feed_post_tag, {id, types = []}).

-record(im_order, {
  id,
  userId,
  created,
  updated,
  productId,
  vendorId,
  qty=0,
  price=0,
  total=0,
  availableUseQty=0,
  type,
  status,
  date,
  purchaseDate,
  expires,
  refundValidTill,
  serial,
  checkoutId
}).

-record(im_order_avail_date_reserve, {
  id,
  userId,
  productId,
  availableDate,
  qty,
  created
}).

-record(im_order_serial_lookup, {id, orderId}).

-record(im_page, {id, title, content}).
-record(im_setting, {id, settings=[]}).

-record(im_user_settings, {settings}).

-record(channel, {
  id,
  owner_id,
  location_id,
  category_id,
  name,
  descr,
  thumb,
  user_count
}).

-record(user_channel_lookup, {id, channelIds=[]}).

-record(channel_category, {id, name}).

-record(channel_post, {
  id,
  author_id,
  author_name,
  author_avatar,
  created_at,
  payload,
  og_data,
  media_type,
  media,
  shares,
  likes,
  thread
}).

-record(channel_post_link, {
  ?ITERATOR(feed),
  post_id,
  channel_id
}).

-record(channel_location, {
  id,
  location_tag,
  name,
  coordinates,
  radius,
  thumb
}).

-record(channel_post_share_lookup, {
  id,
  shares=[]
}).

-record(channel_post_like_lookup, {
  id,
  likes=[]
}).

-record(channel_post_thread, {
  id,
  comments=[]
}).

-record(channel_user_sub_lookup, {
  id,
  users=[]
}).

-record(user_channel_sub_lookup, {
  id,
  channels=[]
}).

-endif.

-record(im_contact, {
  ?ITERATOR(feed),
  userId,
  username,
  name,
  phone,
  originalPhone,
  createdAt=0,
  updatedAt=0,
  thumbnail,
  photo,
  status=2,
  blockedBySystem=false,
  friend=true,
  email,
  skype,
  facebookId,
  vkontakteId,
  departmentId,
  isBot,
  isVendor,
  vendorId,
  bio,
  labels=[]
}).

-record(im_add_contacts_task, {?ITERATOR(feed), userId, deviceId, data, retries}).

-record(im_lookup, {id, userId, contactIds=[]}).
-record(im_lookup_phone, {id, userId, contactIds=[]}).
-record(im_lookup_facebook, {id, userId}).

-record(im_feedback, {
  id,
  userId,
  name,
  client,
  phone,
  email,
  topic,
  message,
  createdAt
}).

-record(im_report, {
  id,
  userId,
  toUserId,
  name,
  client,
  email,
  phone,
  topic,
  information,
  feedId,
  feedType,
  messageEntity,
  createdAt,
  status
}).

-record(im_migration, {
  name,
  time=0
}).

-record(im_ogdata, {
  id,
  domain,
  favicon,
  title,
  description,
  image,
  media=[],
  context,
  json,
  body,
  fetched
}).

-record(im_workflow, {
  id,
  name,
  target,
  version,
  json,
  created,
  updated,
  isSystem
}).

-record(im_workflow_version, {
  id,
  json,
  created,
  updated
}).

-record(im_workflow_state, {
  id,
  name,
  target,
  entityId,
  version,
  step,
  json,
  created,
  updated,
  active
}).
