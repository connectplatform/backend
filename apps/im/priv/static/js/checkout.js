var stripe = Stripe('pk_test_5ktOOD60YlFTnxORUFcFY5sj');
var elements = stripe.elements();
var order;
var product;
var isCharging = false;
let isFormValid = false;

var style = {
  base: {
    fontSize: '16px',
    color: "#32325d",
  }
};

var card = elements.create('card', {style: style});
card.addEventListener('change', ({error}) => {
  const displayError = document.getElementById('card-errors');
  const submitButton = document.getElementById('submit-button');
  isFormValid = !error;

  if (!isFormValid) {
    displayError.textContent = error.message;
    submitButton.setAttribute('disabled', true);
  } else {
    displayError.textContent = '';
    submitButton.removeAttribute('disabled');
  }
});
card.mount('#card-element');

const form = document.getElementById('payment-form');
form.addEventListener('submit', async (event) => {
  event.preventDefault();

  if (!isFormValid) {
    return false;
  }

  const {token, error} = await stripe.createToken(card);

  if (error) {
    const errorElement = document.getElementById('card-errors');
    errorElement.textContent = error.message;
  } else {
    stripeTokenHandler(token);
  }
});

const stripeTokenHandler = token => {
  const orderId = getParameterByName('orderId');

  isCharging = true;

  document.getElementById('form-error').style.display = 'none';
  document.getElementById('loader-wrapper').style.display = 'flex';

  sendMessage('ChargeOrder', {token: token.id, orderIds: [orderId]});
  setTimeout(() => {
    showError('Charge failed due to timeout. Try again later');
  }, 30000);
}

function showError(text) {
  document.getElementById('form-error').style.display = 'block';
  document.getElementById('form-error').innerHTML = text;
  document.getElementById('loader-wrapper').style.display = 'none';
}

function getParameterByName(name) {
  name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
  var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
      results = regex.exec(location.search);
  return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

function onMessage(msg) {
  console.log('Got message: ', msg);

  switch (msg.modelName) {
    case 'OrderResp':
      order = msg.order;
      sendMessage('FeedPost', {postIds: [order.productId]});
      break;
    case 'FeedPostResp':
      product = msg.posts[0];
      render();
      break;
    case 'ChargeOrderResp':
      document.getElementById('successful-payment').style.display = 'flex';
      document.getElementById('loader-wrapper').style.display = 'none';
      break;
    case 'ErrorResp':
      showError(msg.message);
      break;
  }

  if (isCharging && (msg.modelName == 'ChargeOrderResp' || msg.modelName == 'ErrorResp')) {
    isCharging = false;
  }
}

function render() {
  const price = (product.paymentInfo.price / 100).toFixed(2);
  const total = (order.total / 100).toFixed(2);
  document.getElementById('loader-wrapper').style.display = 'none';
  document.getElementById('product-name').innerHTML = product.title;
  document.getElementById('quantity').innerHTML = ' x ' + order.qty;
  document.getElementById('product-price').innerHTML = `${(price || 0)} ${product.paymentInfo.currency}`;
  document.getElementById('product-description').innerHTML = product.descr;
  document.getElementById('total-price').innerHTML = `${(total || 0)} ${product.paymentInfo.currency}`;
}

function sendMessage(name, data) {
  console.log('Sending message: ', name, data);
  ws.send(Hydrator.encode(name, data));
}

var hydratorReady = false;
var ws = Smoothie.connect({
  http: {
    path: "/ws?token=" + getParameterByName("token") + "&version=18-01-2017"
  },
  protocol: "bert",
  onOpen: function() {},
  onMessage: function(data) {
    // console.log("Got message:", Erl.toString(data));
    // console.log("Data:", data);

    if (hydratorReady) {
      onMessage(Hydrator.decode(data));
    } else if (data.value && data.value[0].value == 'AuthResp') {
      Hydrator.fetchMap(() => {
        hydratorReady = true;
        const orderId = getParameterByName('orderId');
        sendMessage('Order', {orderId});
      })
    }
  },
});
