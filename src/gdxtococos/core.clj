(ns gdxtococos.core
 ( :gen-class)
 (:import (javax.imageio ImageIO))
 (:use clojure.xml))

(defn to-sprite-map [sprite]
  (let [pattern #"(\d*),[ ]*(\d*)"
        data (vec (map #(first (re-seq pattern %)) (drop 2 sprite)))
        [_ x y] (data 0)
        [_ w h] (data 1)
        [_ ox oy] (data 2)
        [_ ofx ofy] (data 3)]
    {:name (first sprite) 
     :rotated (last (.split (second sprite) " " ))
     :x x :y y :width w :height h :origx ox :origy oy :offsetx ofx :offsety ofy}))
   
(defn write-plist-header []
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "\n"
       "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">" "\n"
       "<plist version=\"1.0\">" "\n"))

(defn emit-sprite-metadata [imagename w h ]
  {:tag :dict :content
   [{:tag :key :content ["format"]}
    {:tag :integer :content ["2"]}
    {:tag :key :content ["realTextureFileName"]}
    {:tag :string :content [imagename]}
    {:tag :key :content ["size"]}
    {:tag :string :content [(str "{" w "," h "}")]} 
    {:tag :key :content ["textureFileName"]}
    {:tag :string :content [imagename]}]})
  
(defn emit-sprite [{:keys [name width height rotated x y]}]
   [{:tag :key :content [(str name ".png")]}
    {:tag :dict :content
     [{:tag :key :content ["frame"]}
      {:tag :string :content [(str "{{" x "," y "},{" width "," height "}}")]} 
      {:tag :key :content ["offset"]}
      {:tag :string :content ["{0,0}"]}
      {:tag :key :content ["rotated"]}
      {:tag rotated}
      {:tag :key :content ["sourceColorRect"]}
      {:tag :string :content [(str "{{0,0},{" width "," height "}")]}
      {:tag :key :content ["sourceSize"]}
      {:tag :string :content [(str "{" width "," height "}")]}]}])

(defn emit-spritemap-plist [sprites image-name image-width image-height]
  (emit-element {:tag :dict :content
         [{:tag :key :content ["frames"]}
          {:tag :dict :content (vec (mapcat emit-sprite sprites))}
          {:tag :key :content ["metadata"]}
          (emit-sprite-metadata image-name image-width image-height )]}))

(defn write-spritemap-plist [filename sprites image-name image-width image-height]
  (spit filename (str (write-plist-header) (.replace (with-out-str (emit-spritemap-plist sprites image-name image-width image-height)) "\n" "") "</plist>")))
  
(defn convert [filename outname]
  (let [lines (.split (slurp filename) "\n")
        file (java.io.File. filename)
        path (apply str (drop-last (count (.getName file)) (.getPath file)))
        image-name (first (filter #(.endsWith % "png") lines))
        image (ImageIO/read (java.io.File. (str path image-name)))
        image-width (.getWidth image)
        image-height (.getHeight image)
        sprites (map to-sprite-map (partition  7 (drop 5 (seq lines))))]
    (println "convert" filename "to" outname)
    (write-spritemap-plist outname sprites image-name image-width image-height)
    (println "done!")))

(defn -main [& args]
  (if (not (= (count args) 2))
    (do 
      (println "Converts LibGDX TexturePacker2 files to Cocos2d plist format")
      (println "Usage: java -jar gdxtococos.jar <input>.atlas <output>.plist"))
    (convert (first args) (second args))))

 
