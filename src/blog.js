import "./main.js";

import $ from 'jquery';

// TODO: Take user to top of page after clicking on next button
// TODO: Make screen vertical size constant so it doesn't jump after
//       each press
// TODO: Add page button for small screen size
// TODO: Make buttons link to their own pages
// When switching between the small and big screen pagination gets messed up.

//Pagination for blog
$("#blog-cards-grid").load("./blog-pages/1.html")

var pagination = document.getElementById("pagination");
var pageIndex = 0
var numberOfPages = Number.parseInt(pagination.lastChild.innerHTML);
// get value for number of pages from the pagination bar html. The last
// button contains the numberOfPages. based off of the haskell generated html
// Not true for small pagination though
var prevButton = document.getElementById("prev-page");
var nextButton = document.getElementById("next-page");

prevButton.addEventListener("click", prevButtonAction);
nextButton.addEventListener("click", nextButtonAction);

window.addEventListener('resize', () => {
  document
  .getElementById("small-pagination")
  .children[0]
  .innerHTML = pageIndex + 1
  if (numberOfPages <= 5) {
    var activePaginationButton = pagination.querySelector(".bg-blue");
    activePaginationButton.classList.remove("bg-blue","text-white");
    pagination.children[pageIndex].classList.add("bg-blue","text-white");
  }
});

Array.from(pagination.children).forEach((element) => {
  element.addEventListener('click', () => {
    var pageThisScope = parseInt(element.innerHTML);
    var pageIndexThisScope = pageThisScope - 1;
    var activePaginationButton = pagination.querySelector(".bg-blue");
    activePaginationButton.classList.remove("bg-blue","text-white");
    pagination.children[pageIndexThisScope].classList.add("bg-blue","text-white");
    $("#blog-cards-grid").load("./blog-pages/" + pageThisScope + ".html");
    pageIndex = pageThisScope - 1;
  })
})

function shiftPaginationForward () {
  pagination.children[2].innerHTML = parseInt(pagination.children[2].innerHTML) + 1
  pagination.children[3].innerHTML = parseInt(pagination.children[3].innerHTML) + 1
  pagination.children[4].innerHTML = parseInt(pagination.children[4].innerHTML) + 1
}

function shiftPaginationBackward () {
  pagination.children[2].innerHTML = parseInt(pagination.children[2].innerHTML) - 1
  pagination.children[3].innerHTML = parseInt(pagination.children[3].innerHTML) - 1
  pagination.children[4].innerHTML = parseInt(pagination.children[4].innerHTML) - 1
}

function nextButtonAction () {
  if (pageIndex === numberOfPages - 1) {
    console.log("You are on the last page...");
  } else {
    pageIndex++;
    console.log(pageIndex);
    $("#blog-cards-grid").load("./blog-pages/" + (pageIndex + 1) + ".html");
    if (window.matchMedia("screen and (min-width: 768px)").matches) {
      console.log("You have a large screen!");

      if (numberOfPages > 5) {
        console.log("There are more than five pages.");

        var firstPageButton = pagination.firstChild;
        var lastPageButton = pagination.lastChild;

        var middleFirstPageButton = pagination.children[2];
        var middleMiddlePageButton = middleFirstPageButton.nextSibling;
        var middleLastPageButton = middleMiddlePageButton.nextSibling;

        if (pageIndex === 1) {
          console.log("You were on the first page.");

          firstPageButton
          .classList
          .remove("bg-blue","text-white");

          middleFirstPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === numberOfPages - 1) {
          console.log("You were on the second to last page.");

          middleLastPageButton
          .classList
          .remove("bg-blue","text-white");

          lastPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === Number.parseInt(middleLastPageButton.innerHTML)) {
          console.log("The page you were looking at was in the middle last page button");


          // NO need to add or remove classes. Just change inner numbers
          shiftPaginationForward();
        } else {
          console.log("You were on a normal pagination button");

          var activePaginationButton = pagination.querySelector(".bg-blue");

          activePaginationButton.classList.remove("bg-blue","text-white");

          activePaginationButton.nextSibling.classList.add("bg-blue","text-white");
        }
      } else {
        console.log("There are less than five pages.");

        var activePaginationButton = pagination.querySelector(".bg-blue");

        console.log(activePaginationButton);

        activePaginationButton.classList.remove("bg-blue", "text-white");
        console.log(activePaginationButton);
        activePaginationButton.nextSibling.classList.add("bg-blue", "text-white");
      }
    } else {
      console.log("You have a small screen!");
      document.getElementById("small-pagination").children[0].innerHTML = parseInt(document.getElementById("small-pagination").children[0].innerHTML) + 1;
    }
  }
}

function prevButtonAction () {
  if (pageIndex === 0) {
    console.log("You are on the first page...");
  } else {
    pageIndex--;
    console.log(pageIndex);
    $("#blog-cards-grid").load("./blog-pages/" + (pageIndex + 1) + ".html");
    if (window.matchMedia("screen and (min-width: 768px)").matches) {
      console.log("You have a large screen!");

      if (numberOfPages > 5) {
        console.log("There are more than five pages.");

        var firstPageButton = pagination.firstChild;
        var lastPageButton = pagination.lastChild;

        var middleFirstPageButton = pagination.children[2];
        var middleMiddlePageButton = middleFirstPageButton.nextSibling;
        var middleLastPageButton = middleMiddlePageButton.nextSibling;

        if (pageIndex === numberOfPages - 2) {
          console.log("You were on the last page.");

          lastPageButton
          .classList
          .remove("bg-blue","text-white");

          middleLastPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === 0) {
          console.log("You were on the second page.");

          middleFirstPageButton
          .classList
          .remove("bg-blue","text-white");

          firstPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === Number.parseInt(middleFirstPageButton.innerHTML) - 2) {
          console.log("The page you were looking at was in the middle first page button");


          // NO need to add or remove classes. Just change inner numbers
          shiftPaginationBackward();
        } else {
          console.log("You were on a normal pagination button");

          var activePaginationButton = pagination.querySelector(".bg-blue");

          activePaginationButton.classList.remove("bg-blue","text-white");

          activePaginationButton.previousSibling.classList.add("bg-blue","text-white");
        }
      } else {
        console.log("There are less than five pages.");

        var activePaginationButton = pagination.querySelector(".bg-blue");

        console.log(activePaginationButton);

        activePaginationButton.classList.remove("bg-blue", "text-white");
        console.log(activePaginationButton);
        activePaginationButton.previousSibling.classList.add("bg-blue", "text-white");
      }
    } else {
      console.log("You have a small screen!");
      document.getElementById("small-pagination").children[0].innerHTML = parseInt(document.getElementById("small-pagination").children[0].innerHTML) - 1;
    }
  }
}
