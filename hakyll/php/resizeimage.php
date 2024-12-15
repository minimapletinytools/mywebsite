<?php

function resizeImage($imagePath, $maxWidth) {
    // Check if the image exists
    if (!file_exists($imagePath)) {
        echo "The image does not exist.";
        return;
    }

    // Get image info (width, height, mime type)
    list($originalWidth, $originalHeight, $imageType) = getimagesize($imagePath);

    // Calculate the new dimensions based on the max width, keeping the aspect ratio
    $aspectRatio = $originalHeight / $originalWidth;
    $newWidth = $maxWidth;
    $newHeight = intval($newWidth * $aspectRatio);

    // Create an image resource from the original image
    switch ($imageType) {
        case IMAGETYPE_JPEG:
            $sourceImage = imagecreatefromjpeg($imagePath);
            break;
        case IMAGETYPE_PNG:
            $sourceImage = imagecreatefrompng($imagePath);
            break;
        case IMAGETYPE_GIF:
            $sourceImage = imagecreatefromgif($imagePath);
            break;
        default:
            echo "Unsupported image type.";
            return;
    }

    // Create a new empty image with the new dimensions
    $resizedImage = imagecreatetruecolor($newWidth, $newHeight);

    // Preserve transparency for PNG and GIF images
    if ($imageType == IMAGETYPE_PNG || $imageType == IMAGETYPE_GIF) {
        imagealphablending($resizedImage, false);
        imagesavealpha($resizedImage, true);
    }

    // Resize the original image and copy it into the resized image
    imagecopyresampled($resizedImage, $sourceImage, 0, 0, 0, 0, $newWidth, $newHeight, $originalWidth, $originalHeight);

    // Output the resized image (you can save it to a file if needed)
    header('Content-Type: ' . image_type_to_mime_type($imageType));
    switch ($imageType) {
        case IMAGETYPE_JPEG:
            imagejpeg($resizedImage);
            break;
        case IMAGETYPE_PNG:
            imagepng($resizedImage);
            break;
        case IMAGETYPE_GIF:
            imagegif($resizedImage);
            break;
    }

    // Clean up
    imagedestroy($sourceImage);
    imagedestroy($resizedImage);
}

// Check if the required URL parameters are set
if (isset($_GET['imagePath']) && isset($_GET['maxWidth'])) {
    // Get the image path and max width from the URL arguments
    $imagePath = $_GET['imagePath'];
    $maxWidth = intval($_GET['maxWidth']); // Make sure maxWidth is an integer

    // Call the function to resize the image
    resizeImage($imagePath, $maxWidth);
} else {
    echo "Please provide both 'imagePath' and 'maxWidth' parameters in the URL.";
}

?>