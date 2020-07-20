function(input, output){
    
    # persentase penduduk miskin per kabupaten --------------------------------
    
    output$pendMiskin <- renderPlotly({
        penduduk_miskin <- pend_clean %>% 
            filter(tahun == input$selectPendMiskin) %>% 
            slice_max(penduduk_miskin, n = 10) %>% 
            #select(nama_kota_kabupaten, penduduk_miskin, tahun) %>% 
            group_by(nama_kota_kabupaten) %>% 
            summarise(penduduk_miskin = sum(penduduk_miskin)) %>% 
            arrange(desc(penduduk_miskin)) %>% 
            ungroup() %>% 
            mutate(
                text = paste(nama_kota_kabupaten, ":", penduduk_miskin, '%')
            )
        penduduk_miskin_kab <- ggplot(penduduk_miskin, aes(y = penduduk_miskin,
                                                           x = reorder(nama_kota_kabupaten, penduduk_miskin), text = text)) +
            geom_col(aes(fill = penduduk_miskin), 
                     show.legend = F) + 
            coord_flip() + 
            labs(title = "Persentase Pend. Miskin", 
                 y = "Persentase", x = NULL) + 
            scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))+
            scale_fill_gradient(low = "#88CA98", high = "#043D12") +
            theme(plot.title = element_text(face = "bold", 
                                            size = 14, 
                                            hjust = 0.04), 
                  axis.ticks.y = element_blank(), 
                  panel.background = element_rect(fill = "#ffffff"), 
                  panel.grid.major.x = element_line(colour = "grey"), 
                  axis.line.x = element_line(color = "grey"), 
                  axis.text = element_text(size = 10,
                                           colour = "black"))+
            theme_algoritma
        ggplotly(penduduk_miskin_kab, tooltip = "text")   
    })
    
    # Grafik Penduduk Miskin Per Kab Kota dalam tahun -------------------------
    
    output$grafikPendMiskin <- renderPlotly({
        penduduk_miskin_pertahun <- pend_clean %>% 
            filter(nama_kota_kabupaten == input$selectMiskinKab) %>% 
            #select(nama_kota_kabupaten, penduduk_miskin, tahun) %>% 
            group_by(tahun) %>% 
            summarise(penduduk_miskin = sum(penduduk_miskin)) %>% 
            arrange(desc(penduduk_miskin)) %>% 
            ungroup() 
        
        plot_penduduk_miskin_pertahun<- ggplot(penduduk_miskin_pertahun, aes(x = tahun, y = penduduk_miskin))+
            geom_line(color ="Red")+
            geom_point(color = "#771403")+
            scale_x_continuous(breaks=seq(2010,2016, 1))+
            labs(title = "Persentasi Pend Miskin Per Kab/Kota (%)",
                 y = "Persentase", x = "Tahun")+
            theme_algoritma
        
        ggplotly(plot_penduduk_miskin_pertahun)
    })
    # Proyeksi Jumlah Penduduk Jabar ------------------------------------------
    output$proyeksiJmlPend <- renderPlotly({
        jumlah_penduduk_jabar <- jml_pend_clean %>%
            filter(tahun == input$selectYear) %>% 
            #select(nama_kota_kabupaten, proyeksi_jumlah_penduduk, tahun) %>% 
            group_by(nama_kota_kabupaten) %>% 
            summarise(proyeksi_jumlah_penduduk = proyeksi_jumlah_penduduk * 1000) %>% 
            ungroup() %>% 
            mutate(
                text = paste(nama_kota_kabupaten, ":", proyeksi_jumlah_penduduk)
            )
        
        plot_jumlah_penduduk_jabar <- ggplot(jumlah_penduduk_jabar, aes(y = proyeksi_jumlah_penduduk,
                                                                        x = reorder(nama_kota_kabupaten, proyeksi_jumlah_penduduk), text = text))+
            geom_col(aes(fill = proyeksi_jumlah_penduduk), show.legend = F)+
            scale_y_continuous(breaks=seq(0,7000000, 500000))+
            labs(title = "Proyeksi Jumlah Pend. Jabar",
                 y = "Jumlah Penduduk", x = NULL)+
            scale_fill_gradient(low = "#A2AFDE", 
                                high = "#07195D")+
            theme(plot.title = element_text(face = "bold", 
                                            size = 14, 
                                            hjust = 0.04), 
                  axis.ticks.y = element_blank(), 
                  panel.background = element_rect(fill = "#ffffff"), 
                  panel.grid.major.x = element_line(colour = "grey"), 
                  axis.line.x = element_line(color = "grey"), 
                  axis.text = element_text(size = 10,
                                           colour = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 0.5))+
            theme_algoritma
        
        ggplotly(plot_jumlah_penduduk_jabar, tooltip = "text")
    })
    
    # Perbandingan Penduduk Miskin & Sejahtera (%) ----------------------------
    output$pendMiskinAndSejahtera <- renderPlotly({
        penduduk <- merge_penduduk %>% 
            filter(tahun == input$selectPendMiskin) %>% 
            #select(nama_kota_kabupaten, tahun, penduduk_miskin, proyeksi_jumlah_penduduk) %>% 
            mutate(penduduk_sejahtra = 100 - penduduk_miskin,
                   text = paste(nama_kota_kabupaten, ":", penduduk_sejahtra, "%")
            ) %>% 
            group_by(nama_kota_kabupaten) %>% 
            summarise(
                Sejahtera = mean(penduduk_sejahtra),
                Miskin = mean(penduduk_miskin)) %>% 
            ungroup() %>% 
            pivot_longer(cols = -nama_kota_kabupaten) %>% 
            mutate(text = paste(name, ":", value, "%"))
        
        
        plot_penduduk <- ggplot(penduduk, aes(value, nama_kota_kabupaten, text = text))+
            geom_col(aes(fill = name))+
            geom_vline(xintercept = 50, linetype = "dotted")+
            labs(x = NULL, y= NULL, title = "Persentasi Jumlah Penduduk")+
            theme(legend.position = "none") +
            scale_x_continuous(labels = scales::unit_format(unit = "%"))+
            theme_algoritma
        
        ggplotly(plot_penduduk, tooltip = "text") %>%
            config(displayModeBar = F)
    })


# Proyeksi Pertumbuhan Penduduk Jabar Per Tahun ---------------------------
    output$pertumbuhanPendJabar <- renderPlotly({
        penduduk_jabar_pertahun <- jml_pend_clean %>% 
            filter(nama_kota_kabupaten == input$selectKab) %>% 
            #select(nama_kota_kabupaten, penduduk_miskin, tahun) %>% 
            group_by(tahun) %>% 
            summarise(proyeksi_jumlah_penduduk = sum(proyeksi_jumlah_penduduk * 1000)) %>% 
            arrange(desc(proyeksi_jumlah_penduduk)) %>% 
            ungroup() 
        
        plot_jml_pend_pertahun<- ggplot(penduduk_jabar_pertahun, aes(x = tahun, y = proyeksi_jumlah_penduduk))+
            geom_line(color ="#0C2990")+
            geom_point(color = "#01114B")+
            #scale_y_continuous(breaks=seq(40000000,50000000, 500000))+
            scale_x_continuous(breaks=seq(2010, 2016, 1))+
            #scale_y_continuous(breaks = seq(3, 4, 0.25))+
            labs(title = "Pertumbuhan Jumlah Per Tahun",
                 y = "Pertumbuhan Penduduk", x = "Tahun")+
            theme_algoritma
        
        ggplotly(plot_jml_pend_pertahun)
    })

    
# Value Box Proyeksi Jumlah Pendudulk jabar -------------------------------
        output$progressBox <- renderValueBox({
            jml_pend_clean %>% 
                filter(tahun == "2016") %>% 
                summarise(proyeksi_jumlah_penduduk = round(sum(proyeksi_jumlah_penduduk / 1000), 2), "Juta") %>% 
                valueBox(
                paste0("Jumlah Penduduk Th 2016"),
                color = "blue"
                )
        })
        
        output$statusBox <- renderValueBox({
            jml_pend_clean %>% 
            summarise(pertumbuhan = round((47379610 - (sum(proyeksi_jumlah_penduduk) / 7 * 1000)) / 1000000, ), "%") %>% 
            valueBox(
                paste0("Rasio Pertumbuhan Penduduk/Th"),
                color = "yellow"
            )
        })
        
        output$jumlahBox <- renderValueBox({
            jml_pend_clean %>% 
                summarise(pertumbuhan = round((47379610 - 43227380)/7), ) %>% 
            valueBox(
                paste0("Rata Pertumbuhan Penduduk/Th"),
                color = "purple"
            )
        })

# Source Data -------------------------------------------------------------
        output$dataPendJabar <-  DT::renderDataTable({
            jml_pend
        })
        
        output$dataPendMiskin <- DT::renderDataTable({
            pend_clean
        })
        
        url1 <- a("Klik di sini", href="https://data.jabarprov.go.id/dataset/proyeksi-jumlah-penduduk")
        output$tab1 <- renderUI({
            tagList("Data Proyeksi Penduduk Jabar:", url1)
        })
        
        url2 <- a("Klik di sini", href="https://data.jabarprov.go.id/dataset/persentase-penduduk-miskin-berdasarkan-kota-kabupaten")
        output$tab2 <- renderUI({
            tagList("Data Penduduk Miskin Jabar:", url2)
        })
        
        
        

        

}