let socket = null;
let send = null;
let indexListComponent = null;
let posts = [];
let imageUpload = null;
let map = null;

requirejs(['./js/socketConnect', './js/imageUpload'], (socketConnection, imgUpload) => {
  socket = socketConnection.connect(onMessage);
  send = socketConnection.send;
  imageUpload = imgUpload;
});
$.getJSON("/static/data/map.json", json => map = json);

function showLoader() {
  document.getElementById('loader-wrapper').style.display = 'flex';
}
function hideLoader() {
  document.getElementById('loader-wrapper').style.display = 'none';
}

function onMessage(message) {
  console.log(message);

  if (message.auth) {
    return send('FeedPost', {type: 'tickets'});
  }

  switch (message.modelName) {
    // fetch ticket list
    case 'FeedPostResp': {
      hideLoader();
      indexListComponent.items = message.posts || [];
      break;
    }
    case 'FeedPostCategoryResp': editItemComponent.categories = message.categories; break;
    case 'FeedPostDeleteResp':
    case 'FeedPostCreateResp':
    case 'FeedPostUpdateResp': {
      send('FeedPost', {type: 'tickets'});
      editItemComponent.show = false;
      indexListComponent.show = true;
      break;
    }
    case 'UploadMediaResp': {
      if (message.ref === 0) {
        if (!editItemComponent.item.media) {
          editItemComponent.item.media = [];
        }
        editItemComponent.item.media.push({
          height: message.media.height,
          width: message.media.width,
          link: message.url,
          name: message.media.name,
        });
      } else {
        (editItemComponent.item.media || [])
          .find(m => m.name === message.media.name)
          .thumbnail = message.url;
      }
      break;
    }
  }
}

indexListComponent = new Vue({
  el: '#items-list',
  data: {items: [], show: true},
  methods: {
    remove: (ticketId, title) => {
      if (confirm(`Are you sure want to delete '${title}'?`)) {
        showLoader();
        send('FeedPostDelete', {id: ticketId});
      }
    },
    edit: item => {
      showLoader();
      indexListComponent.show = false;
      editItemComponent.init(item);
    },
    create: () => {
      indexListComponent.show = false;
      editItemComponent.init();
    }
  }
});

editItemComponent = new Vue({
  el: '#item-edit',
  data: {item: null, show: false, categories: [], isEdit: true, hasDates: false},
  updated: function () {
    $('.dropdown').dropdown();
    $('.checkbox').checkbox();
    $('.ui.form').form({
      on: 'blur',
      fields: {
        title: 'minLength[5]',
        address: 'minLength[5]',
        description: 'minLength[5]',
      }
    });
  },
  computed: {
    selectedCategories: function () {
      if (!this.categories || !this.item.categories) {
        return '';
      }

      const selected = this.item.categories
        .map(catId => {
          const found = this.categories.find(c => c.id === catId);
          if (found) {
            return found.id;
          }
        });

      return selected.join(',');
    },
    availableCategories: function () {
      return this.categories;
    },
  },
  methods: {
    init: function (item) {
      if (item) {
        this.item = item;
        this.isEdit = true;
        this.hasDates = item.availableDates && item.availableDates.length;
      } else {
        this.isEdit = false;
        // create item stub
        this.item = {
          paymentInfo: {
            availableDates: [], discount: 0, type: map.FeedPostPaymentType.ticket, currency: 'EUR'
          },
          media: [],
          type: 'tickets'
        };
      }
      if (this.item.paymentInfo.price) {
        this.item.paymentInfo.price = (this.item.paymentInfo.price / 100).toFixed(2);
      }

      this.show = true;
      send('FeedPostCategory', {tag: 'store'});
      send('FeedPostTag', {});
      hideLoader();
    },
    isFormValid: function() {
      return isFormValid();
    },
    dateFormat: function(d) {
      return formatDate(new Date(d));
    },
    addAvailableDate: function() {
      if (!this.item.paymentInfo.availableDates) {
        this.item.paymentInfo.availableDates = [];
      }
      this.item.paymentInfo.availableDates.push({date: new Date().getTime(), availableQty: 0});
    },
    removeDate: function(dIndex) {
      if (confirm(`Are you sure want to remove this date?`)) {
        this.item.paymentInfo.availableDates.splice(dIndex, 1);
      }
    },
    dateChanged: function(event, index) {
      if (event.target.value) {
        this.item.paymentInfo.availableDates[index].date = new Date(event.target.value).getTime();
      } else {
        this.item.paymentInfo.availableDates = [];
      }
    },
    categoryChanged: function(event) {
      this.item.categories = event.target.value.split(',');
    },
    back: () => {
      editItemComponent.item = null;
      editItemComponent.show = false;
      indexListComponent.show = true;
    },
    save: function() {
      if (!this.isFormValid) {
        return;
      }

      showLoader();
      // properly type manage
      this.item.paymentInfo.price = (parseFloat(this.item.paymentInfo.price) * 100) || 0;
      this.item.paymentInfo.discount = parseInt(this.item.paymentInfo.discount);

      if (this.item.paymentInfo.availableDates && this.item.paymentInfo.availableDates.length) {
        this.item.paymentInfo.type = map.FeedPostPaymentType.ticketWithDate;
        this.item.paymentInfo.availableDates.forEach(d => d.availableQty = parseInt(d.availableQty));
      }
      if (this.isEdit) {
        send('FeedPostUpdate', {post: this.item});
      } else {
        send('FeedPostCreate', {post: this.item});
      }
    },
    removeImage: function(imgIndex) {
      if (confirm(`Are you sure want to remove this image?`)) {
        this.item.media.splice(imgIndex, 1);
      }
    },
    onUploadImageSelect: async function (event) {
      const readData = await imageUpload.readImages(event.target.files);
      readData.forEach((one, i) => {
        const file = event.target.files[i];
        const name = new Date().getTime() + file.name;
        const standard = imageUpload.resizeAndFormat(one.ctx, file, one.img, 2000000);
        const thumbnail = imageUpload.resizeAndFormat(one.ctx, file, one.img, 24336);

        [standard, thumbnail].forEach((size, index) => {
          send('UploadMedia', {
            ref: index,
            media: {
              name: name,
              type: 1,
              width: size.width,
              height: size.height,
              media: size.media,
              id: undefined,
              mime: file.type,
              thumbnail: undefined,
              duration: undefined,
            },
          });
        });
      });
    },
  }
});

function isFormValid() {
  return $('.ui.form').form('is valid');
}
function formatDate(date) {
  return date.getUTCFullYear() + '-' +
    ('0' + (date.getUTCMonth() + 1)).slice(-2) + '-' +
    ('0' + date.getUTCDate()).slice(-2) +
    'T' +
    ('0' + date.getUTCHours()).slice(-2) + ':' +
    ('0' + date.getUTCMinutes()).slice(-2);
}