import "./styles.css";
import "./main.js";

import $ from 'jquery';

//Pagination for blog
$("#blog-cards-grid").load("./blog-pages/1.html")

var pagination = document.getElementById("pagination");
var pageIndex = 0
var numberOfPages = Number.parseInt(pagination.lastChild.previousSibling.innerHTML);
// get value for number of pages from the pagination bar html. The last
// button contains the numberOfPages. based off of the haskell generated html
// Not true for small pagination though
var prevButton = document.getElementById("prev-page");
var nextButton = document.getElementById("next-page");

prevButton.addEventListener("click", prevButtonAction);
nextButton.addEventListener("click", nextButtonAction);

console.log();

function shiftPaginationForward () {
  pagination.children[2].innerHTML = String.toString(Integer.parseInt(pagination.children[2].innerHTML) + 1);
  pagination.children[3].innerHTML = String.toString(Integer.parseInt(pagination.children[3].innerHTML) + 1);
  pagination.children[4].innerHTML = String.toString(Integer.parseInt(pagination.children[4].innerHTML) + 1);
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

        firstPageButton = pagination.firstChild.nextSibling;
        lastPageButton = pagination.lastChild.previousSibling;

        middleFirstPageButton = pagination.children[3];
        middleMiddlePageButton = middleFirstPageButton.nextSibling;
        middleLastPageButton = middleMiddlePageButton.nextSibling;

        if (pageIndex === 0) {
          console.log("You were on the first page.");

          firstPageButton
          .classList
          .remove("bg-blue","text-white");



          middleFirstPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === numberOfPages - 2) {
          console.log("You were on the second to last page.");

          middleLastPageButton
          .classList
          .remove("bg-blue","text-white");



          lastPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === Number.parseInt(middleLastPageButton.innerHTML) - 1) {
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
      pagination.children[1].innerHTML = pagination.children[1].innerHTML + 1;
    }
  }
}

function prevButtonAction () {
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

        firstPageButton = pagination.firstChild.nextSibling;
        lastPageButton = pagination.lastChild.previousSibling;

        middleFirstPageButton = pagination.children[3];
        middleMiddlePageButton = middleFirstPageButton.nextSibling;
        middleLastPageButton = middleMiddlePageButton.nextSibling;

        if (pageIndex === 0) {
          console.log("You were on the first page.");

          firstPageButton
          .classList
          .remove("bg-blue","text-white");



          middleFirstPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === numberOfPages - 2) {
          console.log("You were on the second to last page.");

          middleLastPageButton
          .classList
          .remove("bg-blue","text-white");



          lastPageButton
          .classList
          .add("bg-blue","text-white");
        } else if (pageIndex === Number.parseInt(middleLastPageButton.innerHTML) - 1) {
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
      pagination.children[1].innerHTML = pagination.children[1].innerHTML + 1;
    }
  }
}
