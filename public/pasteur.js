const paranoia = 0;

function post(path, params, method='post') {
  const form = document.createElement('form');
  form.method = method;
  form.action = path;
  form.setAttribute('enctype', 'multipart/form-data');
  
  for (const key in params) {
    if (Reflect.has(params, key)) {
      const hiddenField = document.createElement('input');
      hiddenField.type = 'hidden';
      hiddenField.name = key;
      hiddenField.value = params[key];

      form.appendChild(hiddenField);
    }
  }

  document.body.appendChild(form);
  form.submit();
}

function doEncrypt () {
  const key = sjcl.random.randomWords(8, paranoia);

  var form = new FormData(document.getElementById('pasteur'));
  var object = {};
  form.forEach(function (value, key) {
    object[key] = value;
  });

  var encrypted = sjcl.encrypt(key, object["paste"]);
  object["paste"] = encrypted;

  post("/#" + sjcl.codec.base64.fromBits(key), object);
}

function doDecrypt() {
  const key = sjcl.codec.base64.toBits(location.hash.substr(1));
  var ct = document.getElementById("raw").innerHTML;
  var decrypted = sjcl.decrypt(key, ct);

  document.getElementById("output").innerHTML = decrypted;
}
