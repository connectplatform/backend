define(function(){
  return {
    readImages: function(fileList) {
      let filePromises = [];
      const canvas = document.createElement('canvas');
      const ctx = canvas.getContext('2d');

        for (let i = 0; i < fileList.length; i++) {
          const reader = new FileReader();
          filePromises.push(
            new Promise((resolve, reject) => {
              // load file
              reader.onload = function(event) {
                // load file to img
                const img = new Image();
                img.onload = function(){
                  // put img on canvas
                  canvas.width = img.width;
                  canvas.height = img.height;
                  ctx.drawImage(img,0,0);
                  resolve({ctx, img});
                };
                img.src = event.target.result;
              };
              reader.readAsDataURL(fileList[i]);
            }),
          );
        }
      return Promise.all(filePromises);
    },

    resizeAndFormat: (context, file, image, maxSquare = 2000000, minSquare = 0) => {
      const canvas = context.canvas;
      const square = canvas.width * canvas.height;
      let resultWidth = canvas.width;
      let resultHeight = canvas.height;
      const mimeType = file.type;

      if (square > maxSquare) {
        let scaleFactor = Math.sqrt(square / maxSquare);
        resultWidth = Math.round(canvas.width / scaleFactor);
        resultHeight = Math.round(canvas.height / scaleFactor);
      } else if (minSquare && square < minSquare) {
        let scaleFactor = Math.sqrt((minSquare / square) / 2);
        resultWidth = Math.round(canvas.width * scaleFactor);
        resultHeight = Math.round(canvas.height * scaleFactor);
      }

      canvas.width = resultWidth;
      canvas.height = resultHeight;
      context.drawImage(image, 0, 0, resultWidth, resultHeight);
      let imageData = canvas.toDataURL(mimeType);
      canvas.remove();

      return {
        width: resultWidth,
        height: resultHeight,
        media: convertDataURIToBinary(imageData),
      };
    }
  };
});

function convertDataURIToBinary(dataURI) {
  const BASE64_MARKER = ';base64,';
  const base64Index = dataURI.indexOf(BASE64_MARKER) + BASE64_MARKER.length;
  const base64 = dataURI.substring(base64Index);
  const raw = window.atob(base64);
  const rawLength = raw.length;
  const array = new Uint8Array(new ArrayBuffer(rawLength));

  for(let i = 0; i < rawLength; i++) {
    array[i] = raw.charCodeAt(i);
  }
  return array;
}
