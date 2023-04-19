# Copyright 2023 DARWIN EU®
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Save an SVG-image from a html-file
#'
#' @param fileName HTML-Filename.
#' @param fileNameOut Filename of image, either with .pdf or .png extension.
#' @param zoom Zoom factor, default = 3.
#' @param vwidth Width of frame to capture, default = 430.
#' @param selector Default: `NULL`; May be set to `"svg"` only works if the output file is pdf, otherwise use `NULL`
#' @param ... Other potential parameters to be passed to webshot2::webshot()
#'
#' @import checkmate
#' @import webshot2
#'
#' @return Invisibly returns the normalized path to all screenshots taken. The character vector will have a class of '"webshot"'.
#' @export
#'
#' @examples
#' \dontrun{
#'   saveAsFile("webFile.html", "outputFile.png")}
saveAsFile <- function(fileName, fileNameOut, zoom = 3, vwidth = 430, selector = NULL, ...) {
  # Assertions
  checkmate::assertCharacter(x = fileName, pattern = "*.html", null.ok = FALSE)
  checkmate::assertCharacter(x = fileNameOut, pattern = "*.pdf|*.png", null.ok = FALSE)
  checkmate::assertNumeric(x = zoom, lower = 0, len = 1, null.ok = FALSE)
  checkmate::assertNumeric(x = vwidth, lower = 0, len = 1, null.ok = FALSE)

  webshot2::webshot(
    url = fileName,
    file = fileNameOut,
    zoom = zoom,
    vwidth = vwidth,
    ...)
}
