function htmlDecode(input){
    var e = document.createElement('div');
    e.innerHTML = input;
    return e.childNodes[0].nodeValue;
}

function checkUpload(size){
    if(eval('size' + htmlDecode('>') + '200')) {
        var n = size.toFixed(2);
        alert('Your file size is: ' + n +
              'KB, and it is too large to upload! Please try to upload smaller file (200KB or less).');
        document.getElementById('btn').style.display='none';
    } else if (size == 0) {
      // do not show empty files
        document.getElementById('btn').style.display='none';
    } else {
      //alert('File size is OK');
        document.getElementById('btn').style.display='block'
    }
}

function checkSize() {
    var elem = document.getElementById('file');
    var fileSize = elem.files[0].size/1024;
    checkUpload(fileSize);
}