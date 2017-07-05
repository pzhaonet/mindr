d3.json("gtor.json", function(error, data) {
  if (error) throw error;
  markmap('svg#mindmap', data);
  
});

function doSomethingWithData(jsondata) {
  console.log(jsondata);
}

d3.json("gtor.json", doSomethingWithData);

